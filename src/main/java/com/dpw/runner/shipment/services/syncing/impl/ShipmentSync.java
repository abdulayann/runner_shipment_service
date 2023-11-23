package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.stringValueOf;

@Component
@Slf4j
public class ShipmentSync implements IShipmentSync {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    RestTemplate restTemplate;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    IShipmentDao shipmentDao;
    @Autowired
    private IV1Service v1Service;

    @Autowired
    private EmailServiceUtility emailServiceUtility;


    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.shipmentSync}")
    private String SHIPMENT_V1_SYNC_URL;

    @Override
    @Async
    public ResponseEntity<?> sync(ShipmentDetails sd) {
        CustomShipmentSyncRequest temp = new CustomShipmentSyncRequest();

        CustomShipmentSyncRequest cs = modelMapper.map(sd, CustomShipmentSyncRequest.class);
        // First map nested entity that are root level properties in v1
        mapAdditionalDetails(cs, sd);
        mapCarrierDetails(cs, sd);
        mapShipmentServices(cs, sd);

        // Map remaining object so there's no info lost for root -> root properties
        // example Guid
        // assigning root level properties not previously mapped
        mapConsolidationGuids(cs, sd);
        cs.setReferenceNo(sd.getBookingReference());
        cs.setCustom_ShipType(sd.getDirection());
        cs.setContainerType(sd.getShipmentType());
        cs.setStatusString(stringValueOf(sd.getStatus()));
        cs.setSalesAgentId(sd.getSalesAgent());
        cs.setInners(sd.getInnerPacks());
        cs.setInnersUnit(sd.getInnerPackUnit());
        cs.setMarksnNums(sd.getMarksNum());
        cs.setConsolidationReferenceNumber(sd.getConsolRef());
        cs.setChargeable(sd.getChargable());
        cs.setChargableUnit(sd.getChargeableUnit());
        cs.setPacks(sd.getNoOfPacks());

        cs.setLockedByUser(sd.getLockedBy()); // lockedBy also present

        cs.setFinanceClosedByUser(sd.getFinanceClosedBy());

        // Fully auto-mapped entities
        // Events, jobs, referenceNumbers, docs, elDetails, services, notes
        // packing (except OriginName field)

        //Mapping root party objects that are not auto-mapped
        cs.setConsignerParty(mapPartyObject(sd.getConsigner()));
        cs.setConsigneeParty(mapPartyObject(sd.getConsignee()));

        // assigning child entities not automatically mapped
        // entityID also gets assigned as a part of this mapping
        mapTruckDriverDetail(cs, sd);
        cs.setRoutings(convertToList(sd.getRoutingsList(), RoutingsRequestV2.class));
        cs.setReferenceNumbers(convertToList(sd.getReferenceNumbersList(), ReferenceNumbersRequestV2.class));
        cs.setPackings_(convertToList(sd.getPackingList(), PackingRequestV2.class));
        cs.setDocs_(convertToList(sd.getFileRepoList(), FileRepoRequestV2.class));
        cs.setELDetails(convertToList(sd.getElDetailsList(), ElDetailsRequestV2.class));

        // Container missing mappings
        // dgClassString, IsHazardous, MarksnNums
        // PickupAddressJSON and DeliveryAddressJSON (could be renamed for easy mapping)

        cs.setBookingCarriages(convertToList(sd.getBookingCarriagesList(), BookingCarriageRequestV2.class));
        cs.setShipmentId(sd.getShipmentId());
        cs.setGuid(sd.getGuid());

        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(cs).module(SyncingConstants.SHIPMENT).build());
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }
            V1DataSyncResponse response_ = v1Service.v1DataSync(finalCs);
            if (!response_.getIsSuccess()) {
                try {
                    emailServiceUtility.sendEmailForSyncEntity(String.valueOf(sd.getId()), String.valueOf(sd.getGuid()),
                            "Shipment Sync", response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync failure for Shipment Sync " + ex.getMessage());
                }
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });

        return ResponseHelper.buildSuccessResponse(modelMapper.map(cs, CustomShipmentSyncRequest.class));
    }

    @Override
    @Async
    public ResponseEntity<?> syncById(Long shipmentId) {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
        if(shipmentDetails.isPresent()) {
            return sync(shipmentDetails.get());
        }
        else {
            throw new DataRetrievalFailureException("");
        }
    }

    private void mapConsolidationGuids(CustomShipmentSyncRequest response, ShipmentDetails request) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(request.getId());
        List<UUID> req = consoleShipmentMappings.stream()
                .map(item -> {
                    return consolidationDetailsDao.findById(item.getConsolidationId()).get().getGuid()  ;
                })
                .collect(Collectors.toList());
        response.setConsolidationGuids(req);
    }

    private PartyRequestV2 mapPartyObject(Parties sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, PartyRequestV2.class);
    }

    private void mapTruckDriverDetail(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getTruckDriverDetails() == null)
            return;

        List<TruckDriverDetailsRequestV2> req = sd.getTruckDriverDetails().stream()
                .map(item -> {
                    TruckDriverDetailsRequestV2 t;
                    t = modelMapper.map(item, TruckDriverDetailsRequestV2.class);
                    t.setTransporterNameOrg(item.getTransporterName());
                    //ENUM
                    t.setTransporterTypeString(item.getTransporterType().toString());
                    return t;
                })
                .collect(Collectors.toList());

        cs.setTruckDriverDetail(req);

    }

    private void mapCarrierDetails(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getCarrierDetails() == null)
            return;
        modelMapper.typeMap(CarrierDetails.class, CustomShipmentSyncRequest.class)
                .addMappings(mp -> mp.skip(CustomShipmentSyncRequest::setDestination))
                .map(sd.getCarrierDetails(), cs);
        cs.setDestinationName(sd.getCarrierDetails().getDestination());
        cs.setDestinationPortName(sd.getCarrierDetails().getDestinationPort());
        cs.setOriginName(sd.getCarrierDetails().getOrigin());
        cs.setOriginPortName(sd.getCarrierDetails().getOriginPort());

    }

    private void mapAdditionalDetails(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getAdditionalDetails() == null)
            return;
        modelMapper.map(sd.getAdditionalDetails(), cs);
        cs.setReceivingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getReceivingForwarder()));
        cs.setSendingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getSendingForwarder()));
        cs.setTraderOrSupplierParty(mapPartyObject(sd.getAdditionalDetails().getTraderOrSupplier()));
    }

    private void mapShipmentServices(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getServicesList() == null)
            return;
        List<ShipmentServiceRequestV2> res = sd.getServicesList().stream().map(
                i -> {
                    var _service = modelMapper.map(i, ShipmentServiceRequestV2.class);
                    _service.setServiceDurationSpan(i.getServiceDuration());
                    return _service;
                }
        ).toList();
        cs.setServicesList(res);
    }


    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }
    private  <T,P> P convertToClass(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }
}
