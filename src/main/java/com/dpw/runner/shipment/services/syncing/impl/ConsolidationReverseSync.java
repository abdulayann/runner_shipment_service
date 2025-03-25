package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.syncing.Entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationReverseSync;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
@Slf4j
@Generated
public class ConsolidationReverseSync implements IConsolidationReverseSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private SyncConfig syncConfig;
    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Override
    public ResponseEntity<IRunnerResponse> reverseSync(CommonRequestModel commonRequestModel, boolean checkForSync, boolean dataMigration) {
        CustomConsolidationRequest request = (CustomConsolidationRequest) commonRequestModel.getData();
        ConsolidationDetailsRequest response = new ConsolidationDetailsRequest();
        String responseMsg;
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !Boolean.TRUE.equals(syncConfig.IS_REVERSE_SYNC_ACTIVE)) {
                return ResponseHelper.buildSuccessResponse();
            }
            response = modelMapper.map(request, ConsolidationDetailsRequest.class);

            response.setCreditor(mapPartyObjectWithFreetext(request.getCreditor(), request.getIsCreditorFreeTextAddress(), request.getCreditorFreeTextAddress()));
            response.setReceivingAgent(mapPartyObjectWithFreetext(request.getReceivingAgent(), request.getIsReceivingAgentFreeTextAddress(), request.getReceivingAgentFreeTextAddress()));
            response.setSendingAgent(mapPartyObjectWithFreetext(request.getSendingAgent(), request.getIsSendingAgentFreeTextAddress(), request.getSendingAgentFreeTextAddress()));

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
            response.setPackingList(jsonHelper.convertValueToList(syncEntityConversionService.packingsV1ToV2(request.getPackingList()), PackingRequest.class));
            response.setRoutingsList(jsonHelper.convertValueToList(syncEntityConversionService.routingsV1ToV2(request.getRoutingsList()), RoutingsRequest.class));
            mapReverseJobs(response, request);
            response.setContainersList(jsonHelper.convertValueToList(syncEntityConversionService.containersV1ToV2(request.getContainersList()).stream().toList(), ContainerRequest.class));
            response.setConsolidationAddresses(jsonHelper.convertValueToList(syncEntityConversionService.addressesV1ToV2(request.getConsolidationAddresses()), PartiesRequest.class));
            response.setFileRepoList(convertToList(request.getDocsList(), FileRepoRequest.class));

            mapReverseShipmentGuids(response, request);
            response.setGuid(request.getGuid());
            response.setSourceGuid(request.getSourceGuid());
            return consolidationService.completeV1ConsolidationCreateAndUpdate(CommonRequestModel.buildRequest(response), dataMigration, request.getCreatedBy(), request.getCreatedDate());
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
        Set<ShipmentRequest> req = new HashSet<>();
        request.getShipmentGuids().forEach((key, value) -> {
            ShipmentRequest shipmentRequest = new ShipmentRequest();
            shipmentRequest.setGuid(key);
            req.add(shipmentRequest);
        });
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
                .toList();
        response.setJobsList(req);
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

    private void mapReverseArrivalDepartureDetails(ConsolidationDetailsRequest consolidationDetailsRequest, CustomConsolidationRequest customConsolidationRequest) {
        if(customConsolidationRequest == null || customConsolidationRequest.getArrivalDepartureDetails() == null)
            return;

        ArrivalDepartureDetails request = customConsolidationRequest.getArrivalDepartureDetails();

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

        consolidationDetailsRequest.setArrivalDetails(response1);

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

        consolidationDetailsRequest.setDepartureDetails(response2);
    }

    private PartiesRequest mapPartyObjectWithFreetext(PartyRequestV2 sourcePartyObject, Boolean isFreeText, String freeTextAddress) {
        if(sourcePartyObject == null)
            return null;
        PartiesRequest parties = modelMapper.map(sourcePartyObject, PartiesRequest.class);
        if(isFreeText != null && isFreeText) {
            parties.setIsAddressFreeText(true);
            if(parties.getAddressData() == null)
                parties.setAddressData(new HashMap<>());
            parties.getAddressData().put(PartiesConstants.RAW_DATA, freeTextAddress);
        }
        return parties;
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
