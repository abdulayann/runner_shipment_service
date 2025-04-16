package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
@Component
@Slf4j
public class DependentServiceHelper {

    private final JsonHelper jsonHelper;
    private final KafkaProducer producer;
    @Value("${shipmentsKafka.queue}")
    private String senderQueue;
    private final ITrackingServiceAdapter trackingServiceAdapter;
    private final IContainerService containerService;

    DependentServiceHelper(JsonHelper jsonHelper, KafkaProducer producer, ITrackingServiceAdapter trackingServiceAdapter
        , IContainerService containerService) {
        this.jsonHelper = jsonHelper;
        this.producer = producer;
        this.trackingServiceAdapter = trackingServiceAdapter;
        this.containerService = containerService;
    }

    public void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, boolean isCreate, boolean isAutoSellRequired, Set<Containers> oldContainers) {
        pushShipmentDataToKafka(shipmentDetails, isCreate, isAutoSellRequired);
        pushShipmentDataToTrackingServiceAdapter(shipmentDetails);
        try {
            containerService.pushContainersToDependentServices(new ArrayList<>(shipmentDetails.getContainersList()), new ArrayList<>(oldContainers));
        }
        catch (Exception e) {
            log.error("Error producing message due to " + e.getMessage());
        }
    }

    private void pushShipmentDataToTrackingServiceAdapter(ShipmentDetails shipmentDetails) {
        try {
            if(shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Completed.getValue()) || shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())
                    && trackingServiceAdapter.checkIfConsolAttached(shipmentDetails)|| (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT) && !Objects.isNull(shipmentDetails.getMasterBill()))) {
                UniversalTrackingPayload utPayload = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
                List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                if(utPayload != null) {
                    trackingPayloads.add(utPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from shipment with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                }
            }
            if(shipmentDetails.getSource() != null && shipmentDetails.getSource().equals(Constants.API)) {
                var events = trackingServiceAdapter.getAllEvents(shipmentDetails,null, shipmentDetails.getBookingReference());
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(shipmentDetails.getBookingReference(),Constants.SHIPMENT, shipmentDetails.getShipmentId(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from shipment with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,true);
                }
            }
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    private void pushShipmentDataToKafka(ShipmentDetails shipmentDetails, boolean isCreate, boolean isAutoSellRequired) {
        try {
            if(shipmentDetails.getTenantId() == null)
                shipmentDetails.setTenantId(TenantContext.getCurrentTenant());
            if (isStringNullOrEmpty(shipmentDetails.getUpdatedBy()))
                shipmentDetails.setUpdatedBy(UserContext.getUser().getUsername());
            ShipmentRequest shipmentRequest = jsonHelper.convertValue(shipmentDetails, ShipmentRequest.class);
            shipmentRequest.setIsAutoSellRequired(isAutoSellRequired);
            if (shipmentDetails.getStatus() != null && shipmentDetails.getStatus() < ShipmentStatus.values().length)
                shipmentRequest.setShipmentStatus(ShipmentStatus.values()[shipmentDetails.getStatus()].toString());
            KafkaResponse kafkaResponse = producer.getKafkaResponse(shipmentRequest, isCreate);
            kafkaResponse.setTransactionId(UUID.randomUUID().toString());
            log.info("Producing shipment data to kafka with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, StringUtility.convertToString(shipmentDetails.getGuid()));
        }
        catch (Exception e) {
            log.error("Error Producing shipment to kafka, error is due to " + e.getMessage());
        }
    }
}
