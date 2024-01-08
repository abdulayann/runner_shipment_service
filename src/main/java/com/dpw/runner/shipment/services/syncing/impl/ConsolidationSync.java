package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ConsolidationSync implements IConsolidationSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    RestTemplate restTemplate;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    IShipmentDao shipmentDao;

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ISyncService syncService;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.consolidationSync}")
    private String CONSOLIDATION_V1_SYNC_URL;

    @Override
    public ResponseEntity<?> sync(ConsolidationDetails request) {
        CustomConsolidationRequest response = new CustomConsolidationRequest();

        response = modelMapper.map(request, CustomConsolidationRequest.class);

        response.setLockedByUser(request.getLockedBy());
        response.setMsnNumberStr(request.getMsnNumber());
        response.setShipmentType(request.getShipmentType());
        response.setCoLoadBookingRef(request.getCoLoadBookingReference());
        response.setType(request.getConsolidationType());
        response.setContainerType(request.getContainerCategory());
        response.setDOPlaceOfIssueName(request.getDoPlaceOfIssue());
        response.setOriginals(request.getOriginal());
        response.setPrinOtherDocs(request.getPrintOtherDocs());
        response.setReferenceNo(request.getReferenceNumber());

        mapCarrierDetails(response, request);
        mapAchievedQuantities(response, request);
        mapAllocations(response, request);

        response.setPlaceOfIssueString(request.getPlaceOfIssue());

        mapArrivalDepartureDetails(response, request);
        mapTruckDriverDetail(response, request);
        mapPackings(response, request);
        mapJobs(response, request);
        response.setContainersList(syncEntityConversionService.containersV2ToV1(request.getContainersList()));
        response.setDocsList(convertToList(request.getFileRepoList(), FileRepoRequestV2.class));
        response.setRoutingsList(syncEntityConversionService.routingsV2ToV1(request.getRoutingsList()));
        response.setConsolidationAddresses(syncEntityConversionService.addressesV2ToV1(request.getConsolidationAddresses()));

        mapShipmentGuids(response, request);
        if(request.getCreditor() != null && request.getCreditor().getIsAddressFreeText() != null && request.getCreditor().getIsAddressFreeText()){
            response.setIsCreditorFreeTextAddress(true);
            var rawData = request.getCreditor().getAddressData() != null ? request.getCreditor().getAddressData().get("rawData"): null;
            if(rawData!=null)
            response.setCreditorFreeTextAddress(rawData.toString());
        }
        else  response.setIsCreditorFreeTextAddress(false);

        if(request.getReceivingAgent() != null && request.getReceivingAgent().getIsAddressFreeText() != null && request.getReceivingAgent().getIsAddressFreeText()){
            response.setIsReceivingAgentFreeTextAddress(true);
            var rawData = request.getReceivingAgent().getAddressData() != null ? request.getReceivingAgent().getAddressData().get("rawData"): null;
            if(rawData!=null)
                response.setReceivingAgentFreeTextAddress(rawData.toString());
        }
        else response.setIsReceivingAgentFreeTextAddress(false);

        if(request.getSendingAgent() != null && request.getSendingAgent().getIsAddressFreeText() != null && request.getSendingAgent().getIsAddressFreeText()){
            response.setIsSendingAgentFreeTextAddress(true);
            var rawData = request.getSendingAgent().getAddressData() != null ? request.getSendingAgent().getAddressData().get("rawData"): null;
            if(rawData!=null)
                response.setSendingAgentFreeTextAddress(rawData.toString());
        }
        else response.setIsSendingAgentFreeTextAddress(false);

        response.setGuid(request.getGuid());
        String consolidationRequest = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(response).module(SyncingConstants.CONSOLIDATION).build());
//        CompletableFuture.runAsync(commonUtils.withMdc(() -> callSync(consolidationRequest, request.getId(), request.getGuid())), commonUtils.syncExecutorService);
        syncService.callSync(consolidationRequest, request.getId(), request.getGuid(), "Consolidation");
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    @Async
    public void syncLockStatus(ConsolidationDetails consolidationDetails) {
        LockSyncRequest lockSyncRequest = LockSyncRequest.builder().guid(consolidationDetails.getGuid()).lockStatus(consolidationDetails.getIsLocked()).build();
        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(lockSyncRequest).module(SyncingConstants.CONSOLIDATION_LOCK).build());
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }
            V1DataSyncResponse response_ = v1Service.v1DataSync(finalCs);
            if (!response_.getIsSuccess()) {
                try {
                    emailServiceUtility.sendEmailForSyncEntity(String.valueOf(consolidationDetails.getId()), String.valueOf(consolidationDetails.getGuid()),
                            "Consolidation Lock Sync", response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync failure for Shipment Sync " + ex.getMessage());
                }
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });
    }

//    private void callSync(String consolidationRequest, Long id, UUID guid) {
//        retryTemplate.execute(ctx -> {
//            log.info("Current retry : {}", ctx.getRetryCount());
//            if (ctx.getLastThrowable() != null) {
//                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
//            }
//
//            V1DataSyncResponse response_ = v1Service.v1DataSync(consolidationRequest);
//            if (!response_.getIsSuccess()) {
//                try {
//                    emailServiceUtility.sendEmailForSyncEntity(String.valueOf(id), String.valueOf(guid),
//                            "Consolidation", response_.getError().toString());
//                } catch (Exception ex) {
//                    log.error("Not able to send email for sync failure for Consolidation: " + ex.getMessage());
//                }
//            }
//            return ResponseHelper.buildSuccessResponse(response_);
//        });
//
//    }

    private void mapShipmentGuids(CustomConsolidationRequest response, ConsolidationDetails request) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(request.getId());
        List<UUID> req = consoleShipmentMappings.stream()
                .map(item -> {
                    return shipmentDao.findById(item.getShipmentId()).get().getGuid();
                })
                .collect(Collectors.toList());
        response.setShipmentGuids(req);
    }

    private void mapJobs(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getJobsList() == null)
            return;
        List<JobRequestV2> req = request.getJobsList().stream()
                .map(item -> {
                    JobRequestV2 p;
                    p = modelMapper.map(item, JobRequestV2.class);
                    p.setEvents(new ArrayList<>());
                    modelMapper.map(item.getEventsList(), p.getEvents());
                    return p;
                })
                .collect(Collectors.toList());
        response.setJobsList(req);
    }

    private void mapPackings(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getPackingList() == null)
            return;
        List<PackingRequestV2> res = syncEntityConversionService.packingsV2ToV1(request.getPackingList(), request.getContainersList(), null, request.getGuid());
        response.setPackingList(res);
    }

    private void mapTruckDriverDetail(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getTruckDriverDetails() == null)
            return;
        List<TruckDriverDetailsRequestV2> req = request.getTruckDriverDetails().stream()
                .map(item -> {
                    TruckDriverDetailsRequestV2 t;
                    t = modelMapper.map(item, TruckDriverDetailsRequestV2.class);
                    t.setTransporterNameOrg(item.getTransporterName());
                    //ENUM
                    t.setTransporterTypeString(item.getTransporterType().toString());
                    return t;
                })
                .collect(Collectors.toList());
        response.setTruckDriverDetail(req);
    }

    private void mapCarrierDetails(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getCarrierDetails() == null)
            return;
        modelMapper.map(request.getCarrierDetails(), response);
        response.setLastDischargeString(request.getCarrierDetails().getDestination());
        response.setDestinationPortName(request.getCarrierDetails().getDestinationPort());
        response.setFirstLoadString(request.getCarrierDetails().getOrigin());
        response.setOriginPortName(request.getCarrierDetails().getOriginPort());
        response.setCarrier(request.getCarrierDetails().getShippingLine());
        response.setVoyageNumber(request.getCarrierDetails().getVoyage());
    }

    private void mapAchievedQuantities(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getAchievedQuantities() == null)
            return;
        modelMapper.map(request.getAchievedQuantities(), response);
        response.setConsolidatedVolume(request.getAchievedQuantities().getConsolidatedVolume());
        response.setConsolidatiedVolumeUnit(request.getAchievedQuantities().getConsolidatedVolumeUnit());
    }

    private void mapAllocations(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getAllocations() == null)
            return;
        modelMapper.map(request.getAllocations(), response);
        response.setChargeable(request.getAllocations().getChargable());
        response.setIsTemparatureControlled(request.getAllocations().getIsTemperatureControlled());
    }

    private void mapArrivalDepartureDetails(CustomConsolidationRequest response_, ConsolidationDetails request_) {
        if(request_ == null)
            return;

        response_.setArrivalDepartureDetails(new ArrivalDepartureDetails());
        ArrivalDepartureDetails response = response_.getArrivalDepartureDetails();

        // Arrival Details
        com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails request1 = request_.getArrivalDetails();

        if(request1 != null) {
            if(request1.getContainerYardId() != null)
                response.setAcontainerYardId(modelMapper.map(request1.getContainerYardId(), PartyRequestV2.class));
            response.setAfirstArrivalPortArrivalDate(request1.getFirstForeignPortArrivalDate());
            response.setAfirstForeignPort(request1.getFirstForeignPort());
            if(request1.getFirstForeignPortId() != null)
                response.setAfirstArrivalPortId(modelMapper.map(request1.getFirstForeignPortId(), PartyRequestV2.class));
            response.setAlastForeignPortDepartureDate(request1.getLastForeignPortDepartureDate());
            response.setAlastForeignPort(request1.getLastForeignPort());
            if(request1.getLastForeignPortId() != null)
                response.setAlastForeignPortId(modelMapper.map(request1.getLastForeignPortId(), PartyRequestV2.class));
            if(request1.getTransportPortId() != null)
                response.setAtransportPortId(modelMapper.map(request1.getTransportPortId(), PartyRequestV2.class));
            if(request1.getCTOId() != null)
                response.setACTOId(modelMapper.map(request1.getCTOId(), PartyRequestV2.class));
            if(request1.getCFSId() != null)
                response.setACFSId(modelMapper.map(request1.getCFSId(), PartyRequestV2.class));
        }

        // Departure Details
        com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails request2 = request_.getDepartureDetails();

        if(request2 != null) {
            if(request2.getContainerYardId() != null)
                response.setDcontainerYardId(modelMapper.map(request2.getContainerYardId(), PartyRequestV2.class));
            response.setDfirstForeignPortArrivalDate(request2.getFirstForeignPortArrivalDate());
            response.setDfirstForeignPort(request2.getFirstForeignPort());
            if(request2.getFirstForeignPortId() != null)
                response.setDfirstForeignPortId(modelMapper.map(request2.getFirstForeignPortId(), PartyRequestV2.class));
            response.setDlastForeignPortDepartureDate(request2.getLastForeignPortDepartureDate());
            response.setDlastForeignPort(request2.getLastForeignPort());
            if(request2.getLastForeignPortId() != null)
                response.setDlastForeignPortId(modelMapper.map(request2.getLastForeignPortId(), PartyRequestV2.class));
            if(request2.getTransportPortId() != null)
                response.setDtransportPortId(modelMapper.map(request2.getTransportPortId(), PartyRequestV2.class));
            if(request2.getCTOId() != null)
                response.setDCTOId(modelMapper.map(request2.getCTOId(), PartyRequestV2.class));
            if(request2.getCFSId() != null)
                response.setDCFSId(modelMapper.map(request2.getCFSId(), PartyRequestV2.class));
        }
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
