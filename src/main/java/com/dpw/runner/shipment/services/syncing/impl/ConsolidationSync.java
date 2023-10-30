package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ArrivalDepartureDetailsRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

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
    RestTemplate restTemplate;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    IShipmentDao shipmentDao;

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

        response.setShipmentType(request.getShipmentType());
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

        response.setGuid(request.getGuid());
        String consolidationRequest = jsonHelper.convertToJson(response);
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            HttpEntity<V1DataResponse> entity = new HttpEntity(consolidationRequest, V1AuthHelper.getHeaders());
            var response_ = this.restTemplate.postForEntity(this.CONSOLIDATION_V1_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
            return response_;
        });

        return ResponseHelper.buildSuccessResponse(response);
    }

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
