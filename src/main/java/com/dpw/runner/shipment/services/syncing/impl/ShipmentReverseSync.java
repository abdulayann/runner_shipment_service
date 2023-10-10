package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ShipmentReverseSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    ShipmentService shipmentService;

    public ResponseEntity<?> reverseSync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CustomShipmentSyncRequest cs = (CustomShipmentSyncRequest) commonRequestModel.getData();
            ShipmentDetails sd = modelMapper.map(cs, ShipmentDetails.class);

            mapCarrierDetailsReverse(cs, sd);
            mapAdditionalDetailsReverse(cs, sd);
            mapReverseShipmentGuids(sd, cs);

//            // Clarity required
//            if(cs.getStatusString() != null && !cs.getStatusString().isEmpty()){
//                sd.setStatus(Integer.parseInt(cs.getStatusString())); // ENUM MAPPING ?
//            }
            sd.setLockedBy(cs.getLockedByUser());

            sd.setBookingReference(cs.getReferenceNo());
            sd.setDirection(cs.getCustom_ShipType());
            sd.setShipmentType(cs.getContainerType());
            sd.setSalesAgent(cs.getSalesAgentId());
            sd.setInnerPacks(cs.getInners());
            sd.setInnerPackUnit(cs.getInnersUnit());
            sd.setMarksNum(cs.getMarksnNums());
            sd.setConsolRef(cs.getConsolidationReferenceNumber());
            sd.setChargable(cs.getChargeable());
            sd.setChargeableUnit(cs.getChargableUnit());
            sd.setNoOfPacks(cs.getPacks());
            sd.setFinanceClosedBy(cs.getFinanceClosedByUser());

            sd.setConsigner(mapPartyObject(cs.getConsignerParty()));
            sd.setConsignee(mapPartyObject(cs.getConsigneeParty()));

            mapTruckDriverDetailReverse(cs, sd);
            sd.setRoutingsList(convertToList(cs.getRoutings(), Routings.class));
            sd.setReferenceNumbersList(convertToList(cs.getReferenceNumbers(), ReferenceNumbers.class));
            sd.setPackingList(convertToList(cs.getPackings_(), Packing.class));
            sd.setFileRepoList(convertToList(cs.getDocs_(), FileRepo.class));
            sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

            sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));

            return shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.
                    buildRequest(modelMapper.map(sd, ShipmentRequest.class)));
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void mapReverseShipmentGuids(ShipmentDetails response, CustomShipmentSyncRequest request) {
        if(request == null || request.getConsolidationGuids() == null)
            return;
        List<ConsolidationDetails> req = request.getConsolidationGuids().stream()
                .map(item -> {
                    ConsolidationDetails p = new ConsolidationDetails();
                    p.setGuid(item);
                    return p;
                })
                .collect(Collectors.toList());
        response.setConsolidationList(req);
    }

    private void mapTruckDriverDetailReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(cs.getTruckDriverDetail() == null)
            return;

        List<TruckDriverDetails> req = cs.getTruckDriverDetail().stream()
                .map(item -> {
                    TruckDriverDetails t;
                    t = modelMapper.map(item, TruckDriverDetails.class);
                    t.setTransporterName(item.getTransporterNameOrg());
                    t.setTransporterType(Ownership.valueOf(item.getTransporterTypeString()));
                    return t;
                })
                .toList();
        sd.setTruckDriverDetails(req);
    }

    private void mapCarrierDetailsReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        // Destination shipment ID is long string source will cause problems
        cs.setShipmentId(null);

        CarrierDetails carrierDetails = modelMapper.map(cs, CarrierDetails.class);
        carrierDetails.setDestination(cs.getDestinationName());
        carrierDetails.setDestinationPort(cs.getDestinationPortName());
        carrierDetails.setOrigin(cs.getOriginName());
        carrierDetails.setOriginPort(cs.getOriginPortName());
        sd.setCarrierDetails(carrierDetails);
    }

    private void mapAdditionalDetailsReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        AdditionalDetails additionalDetails = modelMapper.map(cs, AdditionalDetails.class);
        additionalDetails.setReceivingForwarder(mapPartyObject(cs.getReceivingForwarderParty()));
        additionalDetails.setSendingForwarder(mapPartyObject(cs.getSendingForwarderParty()));
        additionalDetails.setTraderOrSupplier(mapPartyObject(cs.getTraderOrSupplierParty()));
        sd.setAdditionalDetails(additionalDetails);
    }

    private Parties mapPartyObject(PartyRequestV2 sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, Parties.class);
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
