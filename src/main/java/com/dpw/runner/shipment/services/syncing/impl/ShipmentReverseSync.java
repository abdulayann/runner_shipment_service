package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
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
    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    public ResponseEntity<IRunnerResponse> reverseSync(CommonRequestModel commonRequestModel, boolean checkForSync, boolean dataMigration) {
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
            sd.setSourceGuid(cs.getSourceGuid());

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
            sd.setClientCountry(cs.getClientCountryFilter());
            sd.setConsigneeCountry(cs.getConsigneeCountryFilter());
            sd.setConsignorCountry(cs.getConsignorCountryFilter());
            sd.setNotifyPartyCountry(cs.getNotifyPartyCountryFilter());
            sd.setShipmentCreatedOn(cs.getCreatedDate());
            sd.setVolumetricWeight(cs.getVolumeWeight());
            sd.setVolumetricWeightUnit(cs.getWeightVolumeUnit());
            if(!IsStringNullOrEmpty(cs.getPrevShipmentStatusString()))
                sd.setPrevShipmentStatus(ShipmentStatus.valueOf(cs.getPrevShipmentStatusString()).getValue());
            if(!IsStringNullOrEmpty(cs.getStatusString()))
                sd.setStatus(ShipmentStatus.valueOf(cs.getStatusString()).getValue());

            sd.setConsigner(mapPartyObject(cs.getConsignerParty()));
            sd.setConsignee(mapPartyObject(cs.getConsigneeParty()));

            mapTruckDriverDetailReverse(cs, sd);
            sd.setRoutingsList(syncEntityConversionService.routingsV1ToV2(cs.getRoutings()));
            sd.setContainersList(syncEntityConversionService.containersV1ToV2(cs.getContainersList()));
            sd.setShipmentAddresses(syncEntityConversionService.addressesV1ToV2(cs.getShipmentAddresses()));
            sd.setReferenceNumbersList(convertToList(cs.getReferenceNumbers(), ReferenceNumbers.class));
            Map<UUID, String> map = new HashMap<>();
            if(cs.getPackings_() != null)
                map = cs.getPackings_().stream().filter(x-> x.getContainerNumber() != null).collect(Collectors.toMap(PackingRequestV2::getGuid, PackingRequestV2::getContainerNumber));
            sd.setPackingList(syncEntityConversionService.packingsV1ToV2(cs.getPackings_()));
            sd.setFileRepoList(convertToList(cs.getDocs_(), FileRepo.class));
            sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

            sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));
            sd.setGoodsDescription(cs.getDescription());

            List<NotesRequest> customerBookingNotes = convertToList(cs.getCustomerBookingNotesList(), NotesRequest.class);
            return shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.
                    buildRequest(modelMapper.map(sd, ShipmentRequest.class)), map, customerBookingNotes, dataMigration);
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
            if(additionalDetails.getOwnership().equals(Ownership.Self))
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
        if(cs.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            additionalDetails.setDateOfIssue(cs.getIssueDate());
        else
            additionalDetails.setDateOfIssue(cs.getDateofIssue());
        additionalDetails.setDateOfReceipt(cs.getDateofReceipt());
        additionalDetails.setBLChargesDisplay(cs.getChargesApply());
        additionalDetails.setBLExporterShipment(cs.getExporterStmt());
        additionalDetails.setPlaceOfIssue(cs.getPlaceOfIssueName());
        additionalDetails.setPlaceOfSupply(cs.getPlaceOfSupplyName());
        additionalDetails.setPaidPlace(cs.getPaidPlaceName());
        additionalDetails.setCIFValue(cs.getCIFValue());
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
