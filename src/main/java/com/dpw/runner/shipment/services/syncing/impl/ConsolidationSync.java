package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.Entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

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
    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Autowired
    private V1AuthHelper v1AuthHelper;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.consolidationSync}")
    private String CONSOLIDATION_V1_SYNC_URL;

    @Override
    public ResponseEntity<IRunnerResponse> sync(ConsolidationDetails request, String transactionId, boolean isDirectSync) throws RunnerException {
        CustomConsolidationRequest response = createConsoleSyncReq(request);
        String consolidationRequest = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(response).module(SyncingConstants.CONSOLIDATION).build());
        if (isDirectSync) {
            HttpHeaders httpHeaders = v1AuthHelper.getHeadersForDataSyncFromKafka(UserContext.getUser().getUsername(), TenantContext.getCurrentTenant());
            syncService.callSyncAsync(consolidationRequest, StringUtility.convertToString(request.getId()), StringUtility.convertToString(request.getGuid()), "Consolidation", httpHeaders);
        }
        else
            syncService.pushToKafka(consolidationRequest, StringUtility.convertToString(request.getId()), StringUtility.convertToString(request.getGuid()), "Consolidation", transactionId);
       return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public void syncLockStatus(ConsolidationDetails consolidationDetails) {
        LockSyncRequest lockSyncRequest = LockSyncRequest.builder().guid(consolidationDetails.getGuid()).lockStatus(consolidationDetails.getIsLocked()).build();
        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(lockSyncRequest).module(SyncingConstants.CONSOLIDATION_LOCK).build());
        syncService.pushToKafka(finalCs, String.valueOf(consolidationDetails.getId()), String.valueOf(consolidationDetails.getGuid()), "Consolidation Lock Sync", StringUtility.convertToString(consolidationDetails.getGuid()));
    }

    @Override
    public CustomConsolidationRequest createConsoleSyncReq(ConsolidationDetails request) {
        CustomConsolidationRequest response = modelMapper.map(request, CustomConsolidationRequest.class);

        response.setSourceGuid(request.getSourceGuid());
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
        mapPackings(response, request);
        mapJobs(response, request);
        response.setContainersList(syncEntityConversionService.containersV2ToV1(request.getContainersList()));
        response.setDocsList(convertToList(request.getFileRepoList(), FileRepoRequestV2.class));
        response.setRoutingsList(syncEntityConversionService.routingsV2ToV1(request.getRoutingsList()));
        response.setConsolidationAddresses(syncEntityConversionService.addressesV2ToV1(request.getConsolidationAddresses()));
        if(response.getAutoUpdateGoodsDesc() == null)
            response.setAutoUpdateGoodsDesc(false);

        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(request.getId());
        if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()) {
            List<Long> shipmentIds = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).collect(toList());
            ListCommonRequest listReq = constructListCommonRequest("id", shipmentIds, "IN");
            Pair<Specification<ShipmentDetails>, Pageable> pair1 = fetchData(listReq, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair1.getLeft(), pair1.getRight());
            if(shipmentDetailsPage != null && !shipmentDetailsPage.isEmpty()) {
                var map = shipmentDetailsPage.getContent().stream().collect(toMap(ShipmentDetails::getId, ShipmentDetails::getGuid));
                response.setShipmentGuids(map.values().stream().toList());
                mapTruckDriverDetail(response, request, shipmentIds, map);
            }
        }

        if(request.getCreditor() != null && request.getCreditor().getIsAddressFreeText() != null && request.getCreditor().getIsAddressFreeText()){
            response.setIsCreditorFreeTextAddress(true);
            var rawData = request.getCreditor().getAddressData() != null ? request.getCreditor().getAddressData().get(PartiesConstants.RAW_DATA): null;
            if(rawData!=null)
                response.setCreditorFreeTextAddress(rawData.toString());
        }
        else  response.setIsCreditorFreeTextAddress(false);

        if(request.getReceivingAgent() != null && request.getReceivingAgent().getIsAddressFreeText() != null && request.getReceivingAgent().getIsAddressFreeText()){
            response.setIsReceivingAgentFreeTextAddress(true);
            var rawData = request.getReceivingAgent().getAddressData() != null ? request.getReceivingAgent().getAddressData().get(PartiesConstants.RAW_DATA): null;
            if(rawData!=null)
                response.setReceivingAgentFreeTextAddress(rawData.toString());
        }
        else response.setIsReceivingAgentFreeTextAddress(false);

        if(request.getSendingAgent() != null && request.getSendingAgent().getIsAddressFreeText() != null && request.getSendingAgent().getIsAddressFreeText()){
            response.setIsSendingAgentFreeTextAddress(true);
            var rawData = request.getSendingAgent().getAddressData() != null ? request.getSendingAgent().getAddressData().get(PartiesConstants.RAW_DATA): null;
            if(rawData!=null)
                response.setSendingAgentFreeTextAddress(rawData.toString());
        }
        else response.setIsSendingAgentFreeTextAddress(false);

        response.setGuid(request.getGuid());
        return response;
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
                .toList();
        response.setJobsList(req);
    }

    private void mapPackings(CustomConsolidationRequest response, ConsolidationDetails request) {
        if(request == null || request.getPackingList() == null)
            return;
        List<PackingRequestV2> res = syncEntityConversionService.packingsV2ToV1(request.getPackingList(), request.getContainersList(), null, request.getGuid());
        response.setPackingList(res);
    }

    private void mapTruckDriverDetail(CustomConsolidationRequest response, ConsolidationDetails request, List<Long> shipmentIds, Map<Long, UUID> map) {
        List<TruckDriverDetails> truckDriverDetails = null;
        ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentIds, "IN");
        Pair<Specification<TruckDriverDetails>, Pageable> pair = fetchData(listCommonRequest, TruckDriverDetails.class);
        Page<TruckDriverDetails> truckDriverDetailsPage = truckDriverDetailsDao.findAll(pair.getLeft(), pair.getRight());
        truckDriverDetails = truckDriverDetailsPage.stream().toList();
        Map<Long, UUID> contMap = new HashMap<>();
        if(request.getContainersList() != null && !request.getContainersList().isEmpty())
            contMap = request.getContainersList().stream().collect(toMap(Containers::getId, Containers::getGuid));
        Map<Long, UUID> finalContMap = contMap;
        List<TruckDriverDetailsRequestV2> req = truckDriverDetails.stream()
                .map(item -> {
                    TruckDriverDetailsRequestV2 t;
                    t = modelMapper.map(item, TruckDriverDetailsRequestV2.class);
                    t.setTransporterTypeString(item.getTransporterType().toString());
                    t.setConsolidationGuid(request.getGuid());
                    if(item.getShipmentId() != null && map.containsKey(item.getShipmentId()))
                        t.setShipmentGuid(map.get(item.getShipmentId()));
                    if(item.getContainerId() != null && finalContMap.containsKey(item.getContainerId()))
                        t.setContainerGuid(finalContMap.get(item.getContainerId()));
                    return t;
                })
                .toList();
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
                .toList();
    }

    private  <T,P> P convertToClass(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }
}
