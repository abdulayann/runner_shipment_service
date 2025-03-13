package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.response.TIKafkaEventResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaAsyncService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@Service
public class KafkaAsyncService implements IKafkaAsyncService {

    private final JsonHelper jsonHelper;

    private final IShipmentDao shipmentDao;

    private final IConsolidationDetailsDao consolidationDetailsDao;

    private final KafkaProducer producer;

    @Value("${awbKafka.queue}")
    private String awbSenderQueue;

    @Value("${tiKafka.queue}")
    private String tiSenderQueue;

    @Autowired
    public KafkaAsyncService(JsonHelper jsonHelper, IShipmentDao shipmentDao, IConsolidationDetailsDao consolidationDetailsDao, KafkaProducer producer) {
        this.jsonHelper = jsonHelper;
        this.shipmentDao = shipmentDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.producer = producer;
    }

    @Async
    public void pushToKafkaAwb(Awb awb, boolean isCreate) {
        try {
            if(awb.getTenantId() == null)
                awb.setTenantId(TenantContext.getCurrentTenant());
            AwbResponse awbResponse = jsonHelper.convertValue(awb, AwbResponse.class);
            if(awb.getShipmentId() != null) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
                if(shipmentDetails.isPresent())
                    awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(shipmentDetails.get(), AwbShipConsoleDto.class));
                else
                    return;
            } else if (awb.getConsolidationId() != null) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
                if(consolidationDetails.isPresent())
                    awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(consolidationDetails.get(), AwbShipConsoleDto.class));
                else
                    return;
            }
            else {
                return;
            }

            KafkaResponse kafkaResponse = producer.getKafkaResponse(awbResponse, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), awbSenderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing awb to kafka: {}", e.getMessage());
        }
    }

    @Async
    public void pushToKafkaTI(List<IRunnerResponse> pickupDeliveryDetails, boolean isCreate, Long shipmentId) {
        try {
            TIKafkaEventResponse tiKafkaEventResponse = new TIKafkaEventResponse();
            tiKafkaEventResponse.setShipmentId(shipmentId);
            tiKafkaEventResponse.setPickupDeliveryDetails(pickupDeliveryDetails);

            KafkaResponse kafkaResponse = producer.getKafkaResponse(tiKafkaEventResponse, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), tiSenderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing Transport Instructions to kafka: {}", e.getMessage());
        }
    }
}
