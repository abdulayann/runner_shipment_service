package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationReverseSync;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ConsolidationReverseSync implements IConsolidationReverseSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    private IConsolidationService consolidationService;
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;
    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Override
    public ResponseEntity<?> reverseSync(CommonRequestModel commonRequestModel, boolean checkForSync) {
        CustomConsolidationRequest request = (CustomConsolidationRequest) commonRequestModel.getData();
        ConsolidationDetailsRequest response = new ConsolidationDetailsRequest();
        String responseMsg;
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.CONSOLIDATION, StringUtility.convertToString(request.getGuid()), request);
            }
            response = modelMapper.map(request, ConsolidationDetailsRequest.class);

            response.setLockedBy(request.getLockedByUser());
            response.setMsnNumber(request.getMsnNumberStr());
            response.setShipmentType(request.getShipmentType());
            response.setCoLoadBookingReference(request.getCoLoadBookingRef());
            response.setConsolidationType(request.getType());
            response.setContainerCategory(request.getContainerType());
            response.setDoPlaceOfIssue(request.getDOPlaceOfIssueName());
            response.setOriginal(request.getOriginals());
            response.setPrintOtherDocs(request.getPrinOtherDocs());
            response.setReferenceNumber(request.getReferenceNo());

            mapReverseCarrierDetails(response, request);
            mapReverseAchievedQuantities(response, request);
            mapReverseAllocations(response, request);

            response.setPlaceOfIssue(request.getPlaceOfIssueString());

            mapReverseArrivalDepartureDetails(response, request);
            mapReverseTruckDriverDetail(response, request);
            response.setPackingList(jsonHelper.convertValueToList(syncEntityConversionService.packingsV1ToV2(request.getPackingList()), PackingRequest.class));
            response.setRoutingsList(jsonHelper.convertValueToList(syncEntityConversionService.routingsV1ToV2(request.getRoutingsList()), RoutingsRequest.class));
            mapReverseJobs(response, request);
            response.setContainersList(jsonHelper.convertValueToList(syncEntityConversionService.containersV1ToV2(request.getContainersList()), ContainerRequest.class));
            response.setFileRepoList(convertToList(request.getDocsList(), FileRepoRequest.class));

            mapReverseShipmentGuids(response, request);
            response.setGuid(request.getGuid());
            return consolidationService.completeV1ConsolidationCreateAndUpdate(CommonRequestModel.buildRequest(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void mapReverseShipmentGuids(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null || request.getShipmentGuids() == null)
            return;
        List<ShipmentRequest> req = request.getShipmentGuids().stream()
                .map(item -> {
                    ShipmentRequest p = new ShipmentRequest();
                    p.setGuid(item);
                    return p;
                })
                .collect(Collectors.toList());
        response.setShipmentsList(req);
    }

    private void mapReverseJobs(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null || request.getJobsList() == null)
            return;
        List<JobRequest> req = request.getJobsList().stream()
                .map(item -> {
                    JobRequest p;
                    p = modelMapper.map(item, JobRequest.class);
                    p.setEventsList(new ArrayList<>());
                    if(item.getEvents() != null)
                        modelMapper.map(item.getEvents(), p.getEventsList());
                    return p;
                })
                .collect(Collectors.toList());
        response.setJobsList(req);
    }

    private void mapReverseTruckDriverDetail(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null || request.getTruckDriverDetail() == null)
            return;
        List<TruckDriverDetailsRequest> req = request.getTruckDriverDetail().stream()
                .map(item -> {
                    TruckDriverDetailsRequest t;
                    t = modelMapper.map(item, TruckDriverDetailsRequest.class);
                    t.setTransporterName(item.getTransporterNameOrg());
                    //ENUM
                    t.setTransporterType(Ownership.valueOf(item.getTransporterTypeString()));
                    return t;
                })
                .collect(Collectors.toList());
        response.setTruckDriverDetails(req);
    }

    private void mapReverseCarrierDetails(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null)
            return;
        response.setCarrierDetails(new CarrierDetailRequest());
        modelMapper.map(request, response.getCarrierDetails());
        response.getCarrierDetails().setDestination(request.getLastDischargeString());
        response.getCarrierDetails().setDestinationPort(request.getDestinationPortName());
        response.getCarrierDetails().setOrigin(request.getFirstLoadString());
        response.getCarrierDetails().setOriginPort(request.getOriginPortName());
        response.getCarrierDetails().setShippingLine(request.getCarrier());
        response.getCarrierDetails().setGuid(null);
        response.getCarrierDetails().setId(null);
        response.getCarrierDetails().setVoyage(request.getVoyageNumber());
    }

    private void mapReverseAchievedQuantities(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null)
            return;
        response.setAchievedQuantities(new AchievedQuantitiesRequest());
        modelMapper.map(request, response.getAchievedQuantities());
        response.getAchievedQuantities().setConsolidatedVolume(request.getConsolidatedVolume());
        response.getAchievedQuantities().setConsolidatedVolumeUnit(request.getConsolidatiedVolumeUnit());
        response.getAchievedQuantities().setGuid(null);
        response.getAchievedQuantities().setId(null);
    }

    private void mapReverseAllocations(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null)
            return;
        response.setAllocations(new AllocationsRequest());
        modelMapper.map(request, response.getAllocations());
        response.getAllocations().setChargable(request.getChargeable());
        response.getAllocations().setIsTemperatureControlled(request.getIsTemparatureControlled());
        response.getAllocations().setGuid(null);
        response.getAllocations().setId(null);
    }

    private void mapReverseArrivalDepartureDetails(ConsolidationDetailsRequest response_, CustomConsolidationRequest request_) {
        if(request_ == null || request_.getArrivalDepartureDetails() == null)
            return;

        ArrivalDepartureDetails request = request_.getArrivalDepartureDetails();

        // Arrival Details
        ArrivalDepartureDetailsRequest response1 = new ArrivalDepartureDetailsRequest();

        if(request.getAcontainerYardId() != null)
            response1.setContainerYardId(modelMapper.map(request.getAcontainerYardId(), PartiesRequest.class));
        response1.setFirstForeignPortArrivalDate(request.getAfirstArrivalPortArrivalDate());
        response1.setFirstForeignPort(request.getAfirstForeignPort());
        if(request.getAfirstArrivalPortId() != null)
            response1.setFirstForeignPortId(modelMapper.map(request.getAfirstArrivalPortId(), PartiesRequest.class));
        response1.setLastForeignPortDepartureDate(request.getAlastForeignPortDepartureDate());
        response1.setLastForeignPort(request.getAlastForeignPort());
        if(request.getAlastForeignPortId() != null)
            response1.setLastForeignPortId(modelMapper.map(request.getAlastForeignPortId(), PartiesRequest.class));
        if(request.getAtransportPortId() != null)
            response1.setTransportPortId(modelMapper.map(request.getAtransportPortId(), PartiesRequest.class));
        if(request.getACTOId() != null)
            response1.setCTOId(modelMapper.map(request.getACTOId(), PartiesRequest.class));
        if(request.getACFSId() != null)
            response1.setCFSId(modelMapper.map(request.getACFSId(), PartiesRequest.class));

        response_.setArrivalDetails(response1);

        // Departure Details
        ArrivalDepartureDetailsRequest response2 = new ArrivalDepartureDetailsRequest();

        if(request.getDcontainerYardId() != null)
            response2.setContainerYardId(modelMapper.map(request.getDcontainerYardId(), PartiesRequest.class));
        response2.setFirstForeignPortArrivalDate(request.getDfirstForeignPortArrivalDate());
        response2.setFirstForeignPort(request.getDfirstForeignPort());
        if(request.getDfirstForeignPortId() != null)
            response2.setFirstForeignPortId(modelMapper.map(request.getDfirstForeignPortId(), PartiesRequest.class));
        response2.setLastForeignPortDepartureDate(request.getDlastForeignPortDepartureDate());
        response2.setLastForeignPort(request.getDlastForeignPort());
        if(request.getDlastForeignPortId() != null)
            response2.setLastForeignPortId(modelMapper.map(request.getDlastForeignPortId(), PartiesRequest.class));
        if(request.getDtransportPortId() != null)
            response2.setTransportPortId(modelMapper.map(request.getDtransportPortId(), PartiesRequest.class));
        if(request.getDCTOId() != null)
            response2.setCTOId(modelMapper.map(request.getDCTOId(), PartiesRequest.class));
        if(request.getDCFSId() != null)
            response2.setCFSId(modelMapper.map(request.getDCFSId(), PartiesRequest.class));

        response_.setDepartureDetails(response2);
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
