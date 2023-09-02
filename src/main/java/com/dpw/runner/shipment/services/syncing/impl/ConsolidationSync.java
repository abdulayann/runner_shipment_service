package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dto.request.ArrivalDepartureDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class ConsolidationSync implements IConsolidationSync {

    @Autowired
    ModelMapper modelMapper;
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

    private void mapCarrierDetails(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getCarrierDetails() == null)
            return;
        modelMapper.map(request.getCarrierDetails(), response);
        response.setDestinationName(request.getCarrierDetails().getDestination());
        response.setDestinationPortName(request.getCarrierDetails().getDestinationPort());
        response.setOriginName(request.getCarrierDetails().getOrigin());
        response.setOriginPortName(request.getCarrierDetails().getOriginPort());
    }

    private void mapAchievedQuantities(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getAchievedQuantities() == null)
            return;
        modelMapper.map(request.getCarrierDetails(), response);
        response.setConsolidatedVolume(request.getAchievedQuantities().getConsolidatedVolume());
        response.setConsolidatiedVolumeUnit(request.getAchievedQuantities().getConsolidatedVolumeUnit());
    }

    private void mapAllocations(CustomConsolidationRequest response, ConsolidationDetailsRequest request) {
        if(request == null || request.getAllocations() == null)
            return;
        modelMapper.map(request.getAllocations(), response);
        response.setChargeable(request.getAllocations().getChargable());
        response.setIsTemparatureControlled(request.getAllocations().getIsTemperatureControlled());
    }

    private void mapParty(PartyRequestV2 response, PartiesRequest request) {
        if(request == null)
            return;
        response = new PartyRequestV2();
        modelMapper.map(request, response);
    }

    private void mapArrivalDepartureDetails(CustomConsolidationRequest response_, ConsolidationDetailsRequest request_) {
        if(request_ == null || request_.getArrivalDetails() == null)
            return;

        response_.setArrivalDepartureDetails(new ArrivalDepartureDetails());
        ArrivalDepartureDetails response = response_.getArrivalDepartureDetails();

        // Arrival Details
        ArrivalDepartureDetailsRequest request1 = request_.getArrivalDetails();

        mapParty(response.getAcontainerYardId(), request1.getContainerYardId());
        response.setAfirstArrivalPortArrivalDate(request1.getFirstForeignPortArrivalDate());
        response.setAfirstForeignPort(request1.getFirstForeignPort());
        mapParty(response.getAfirstArrivalPortId(), request1.getFirstForeignPortId());
        response.setAlastForeignPortDepartureDate(request1.getLastForeignPortDepartureDate());
        response.setAlastForeignPort(request1.getLastForeignPort());
        mapParty(response.getAlastForeignPortId(), request1.getLastForeignPortId());
        mapParty(response.getAtransportPortId(), request1.getTransportPortId());
        mapParty(response.getACTOId(), request1.getCTOId());
        mapParty(response.getACFSId(), request1.getCFSId());

        // Departure Details
        ArrivalDepartureDetailsRequest request2 = request_.getDepartureDetails();

        mapParty(response.getDcontainerYardId(), request2.getContainerYardId());
        response.setDfirstForeignPortArrivalDate(request2.getFirstForeignPortArrivalDate());
        response.setDfirstForeignPort(request2.getFirstForeignPort());
        mapParty(response.getDfirstForeignPortId(), request2.getFirstForeignPortId());
        response.setDlastForeignPortDepartureDate(request2.getLastForeignPortDepartureDate());
        response.setDlastForeignPort(request2.getLastForeignPort());
        mapParty(response.getDlastForeignPortId(), request2.getLastForeignPortId());
        mapParty(response.getDtransportPortId(), request2.getTransportPortId());
        mapParty(response.getDCTOId(), request2.getCTOId());
        mapParty(response.getDCFSId(), request2.getCFSId());
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
