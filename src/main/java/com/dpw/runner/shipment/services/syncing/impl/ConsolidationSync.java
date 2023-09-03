package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ConsolidationSync implements IConsolidationSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    private IConsolidationService consolidationService;

    @Override
    public ResponseEntity<?> sync(ConsolidationDetailsRequest request) {
        CustomConsolidationRequest response = new CustomConsolidationRequest();
        response.setShipmentType(request.getBookingType());
        response.setCoLoadBookingRef(request.getCoLoadBookingReference());
        response.setType(request.getConsolidationType());
        response.setContainerType(request.getContainerCategory());
        response.setDOPlaceOfIssueName(request.getDoPlaceOfIssue());
        response.setFirstLoadString(request.getFirstLoad());
        response.setLastDischargeString(request.getLastDischarge());
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
        response.setDocsList(convertToList(request.getFileRepoList(), FileRepoRequestV2.class));

        mapShipmentGuids(response, request);
        response = modelMapper.map(request, CustomConsolidationRequest.class);

        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public ResponseEntity<?> reverseSync(CommonRequestModel request_) {
        CustomConsolidationRequest request = (CustomConsolidationRequest) request_.getData();
        ConsolidationDetailsRequest response = new ConsolidationDetailsRequest();
        String responseMsg;
        try {
            response = modelMapper.map(request, ConsolidationDetailsRequest.class);
            response.setBookingType(request.getShipmentType());
            response.setCoLoadBookingReference(request.getCoLoadBookingRef());
            response.setConsolidationType(request.getType());
            response.setContainerCategory(request.getContainerType());
            response.setDoPlaceOfIssue(request.getDOPlaceOfIssueName());
            response.setFirstLoad(request.getFirstLoadString());
            response.setLastDischarge(request.getLastDischargeString());
            response.setOriginal(request.getOriginals());
            response.setPrintOtherDocs(request.getPrinOtherDocs());
            response.setReferenceNumber(request.getReferenceNo());

            mapReverseCarrierDetails(response, request);
            mapReverseAchievedQuantities(response, request);
            mapReverseAllocations(response, request);

            response.setPlaceOfIssue(request.getPlaceOfIssueString());

            mapReverseArrivalDepartureDetails(response, request);
            mapReverseTruckDriverDetail(response, request);
            mapReversePackings(response, request);
            mapReverseJobs(response, request);
            response.setFileRepoList(convertToList(request.getDocsList(), FileRepoRequest.class));

            mapReverseShipmentGuids(response, request);

            return consolidationService.completeV1ConsolidationCreateAndUpdate(CommonRequestModel.buildRequest(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void mapShipmentGuids(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getShipmentsList() == null)
            return;
        List<UUID> req = request.getShipmentsList().stream()
                .map(item -> {
                    return item.getGuid();
                })
                .collect(Collectors.toList());
        response.setShipmentGuids(req);
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

    private void mapJobs(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
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

    private void mapPackings(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getPackingList() == null)
            return;
        List<PackingRequestV2> req = request.getPackingList().stream()
                .map(item -> {
                    PackingRequestV2 p;
                    p = modelMapper.map(item, PackingRequestV2.class);
                    p.setOriginName(item.getOrigin());
                    return p;
                })
                .collect(Collectors.toList());
        response.setPackingList(req);
    }

    private void mapReversePackings(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null || request.getPackingList() == null)
            return;
        List<PackingRequest> req = request.getPackingList().stream()
                .map(item -> {
                    PackingRequest p;
                    p = modelMapper.map(item, PackingRequest.class);
                    p.setOrigin(item.getOriginName());
                    return p;
                })
                .collect(Collectors.toList());
        response.setPackingList(req);
    }

    private void mapTruckDriverDetail(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
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

    private void mapCarrierDetails(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getCarrierDetails() == null)
            return;
        modelMapper.map(request.getCarrierDetails(), response);
        response.setDestinationName(request.getCarrierDetails().getDestination());
        response.setDestinationPortName(request.getCarrierDetails().getDestinationPort());
        response.setOriginName(request.getCarrierDetails().getOrigin());
        response.setOriginPortName(request.getCarrierDetails().getOriginPort());
    }

    private void mapReverseCarrierDetails(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null)
            return;
        response.setCarrierDetails(new CarrierDetailRequest());
        modelMapper.map(request, response.getCarrierDetails());
        response.getCarrierDetails().setDestination(request.getDestinationName());
        response.getCarrierDetails().setDestinationPort(request.getDestinationPortName());
        response.getCarrierDetails().setOrigin(request.getOriginName());
        response.getCarrierDetails().setOriginPort(request.getOriginPortName());
    }

    private void mapAchievedQuantities(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getAchievedQuantities() == null)
            return;
        modelMapper.map(request.getAchievedQuantities(), response);
        response.setConsolidatedVolume(request.getAchievedQuantities().getConsolidatedVolume());
        response.setConsolidatiedVolumeUnit(request.getAchievedQuantities().getConsolidatedVolumeUnit());
    }

    private void mapReverseAchievedQuantities(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null)
            return;
        response.setAchievedQuantities(new AchievedQuantitiesRequest());
        modelMapper.map(request, response.getAchievedQuantities());
        response.getAchievedQuantities().setConsolidatedVolume(request.getConsolidatedVolume());
        response.getAchievedQuantities().setConsolidatedVolumeUnit(request.getConsolidatiedVolumeUnit());
    }

    private void mapAllocations(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getAllocations() == null)
            return;
        modelMapper.map(request.getAllocations(), response);
        response.setChargeable(request.getAllocations().getChargable());
        response.setIsTemparatureControlled(request.getAllocations().getIsTemperatureControlled());
    }

    private void mapReverseAllocations(ConsolidationDetailsRequest response, CustomConsolidationRequest request) {
        if(request == null)
            return;
        response.setAllocations(new AllocationsRequest());
        modelMapper.map(request, response.getAllocations());
        response.getAllocations().setChargable(request.getChargeable());
        response.getAllocations().setIsTemperatureControlled(request.getIsTemparatureControlled());
    }

    private void mapArrivalDepartureDetails(CustomConsolidationRequest response_, ConsolidationDetailsRequest request_) {
        if(request_ == null)
            return;

        response_.setArrivalDepartureDetails(new ArrivalDepartureDetails());
        ArrivalDepartureDetails response = response_.getArrivalDepartureDetails();

        // Arrival Details
        ArrivalDepartureDetailsRequest request1 = request_.getArrivalDetails();

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
        ArrivalDepartureDetailsRequest request2 = request_.getDepartureDetails();

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
