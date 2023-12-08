package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Service
@Slf4j
public class ShipmentReverseSync implements IShipmentReverseSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    IShipmentService shipmentService;
    @Lazy
    @Autowired
    ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;

    public ResponseEntity<?> reverseSync(CommonRequestModel commonRequestModel, boolean checkForSync) {
        String responseMsg;
        try {

            CustomShipmentSyncRequest cs = (CustomShipmentSyncRequest) commonRequestModel.getData();
            ShipmentDetails sd = modelMapper.map(cs, ShipmentDetails.class);

            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.SHIPMENT, StringUtility.convertToString(sd.getGuid()), cs);
            }
            mapCarrierDetailsReverse(cs, sd);
            mapAdditionalDetailsReverse(cs, sd);
            mapReverseShipmentGuids(sd, cs);
            mapShipmentServiceReverse(cs, sd);

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
            Map<UUID, String> map = new HashMap<>();
            if(cs.getPackings_() != null)
                map = cs.getPackings_().stream().collect(Collectors.toMap(PackingRequestV2::getGuid, PackingRequestV2::getContainerNumber));
            sd.setPackingList(convertToList(cs.getPackings_(), Packing.class));
            sd.setFileRepoList(convertToList(cs.getDocs_(), FileRepo.class));
            sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

            sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));
            sd.setGoodsDescription(cs.getDescription());

            return shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.
                    buildRequest(modelMapper.map(sd, ShipmentRequest.class)), map);
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
        carrierDetails.setGuid(null);
        sd.setCarrierDetails(carrierDetails);
    }

    private void mapAdditionalDetailsReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        AdditionalDetails additionalDetails = modelMapper.map(cs, AdditionalDetails.class);
        additionalDetails.setReceivingForwarder(mapPartyObject(cs.getReceivingForwarderParty()));
        additionalDetails.setSendingForwarder(mapPartyObject(cs.getSendingForwarderParty()));
        additionalDetails.setTraderOrSupplier(mapPartyObject(cs.getTraderOrSupplierParty()));
        if(!IsStringNullOrEmpty(cs.getAndesStatusString()))
            additionalDetails.setAndesStatus(AndesStatus.valueOf(cs.getAndesStatusString()));
        if(!IsStringNullOrEmpty(cs.getOwnershipString())) {
            additionalDetails.setOwnership(Ownership.valueOf(cs.getOwnershipString()));
            if(additionalDetails.getOwnership().equals(Ownership.SELF))
                additionalDetails.setOwnershipName(cs.getOwnershipName());
            else
                additionalDetails.setOwnershipOrg(mapPartyObject(cs.getOwnershipParty()));
        }
        if(!IsStringNullOrEmpty(cs.getPassedByString()))
            additionalDetails.setPassedBy(Ownership.valueOf(cs.getPassedByString()));
        additionalDetails.setBOEDate(cs.getBoedate());
        additionalDetails.setBOENumber(cs.getBoenumber());
        additionalDetails.setGuid(null);
        additionalDetails.setDeliveryMode(cs.getHblDeliveryMode());
        sd.setAdditionalDetails(additionalDetails);
    }

    private void mapShipmentServiceReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(cs.getServicesList() == null)
            return;
        List<ServiceDetails> res = cs.getServicesList().stream().map(
                i -> {
                    var _service = modelMapper.map(i, ServiceDetails.class);
                    _service.setServiceDuration(i.getServiceDurationSpan());
                    return _service;
                }
        ).toList();
        sd.setServicesList(res);
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
