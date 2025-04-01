package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
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
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Service
@Slf4j
@Generated
public class ShipmentReverseSync implements IShipmentReverseSync {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    IShipmentService shipmentService;

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
                return ResponseHelper.buildSuccessResponse();
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
            sd.setCreatedAt(cs.getInsertDate());
            sd.setVolumetricWeight(cs.getVolumeWeight());
            sd.setVolumetricWeightUnit(cs.getWeightVolumeUnit());
            if(!IsStringNullOrEmpty(cs.getPrevShipmentStatusString()))
                sd.setPrevShipmentStatus(ShipmentStatus.valueOf(cs.getPrevShipmentStatusString()).getValue());
            if(!IsStringNullOrEmpty(cs.getStatusString()))
                sd.setStatus(ShipmentStatus.valueOf(cs.getStatusString()).getValue());

            sd.setConsigner(mapPartyObjectWithFreetext(cs.getConsignerParty(), cs.getIsConsignerFreeTextAddress(), cs.getConsignerFreeTextAddress()));
            sd.setConsignee(mapPartyObjectWithFreetext(cs.getConsigneeParty(), cs.getIsConsigneeFreeTextAddress(), cs.getConsigneeFreeTextAddress()));

            mapTruckDriverDetailReverse(cs, sd);
            sd.setRoutingsList(syncEntityConversionService.routingsV1ToV2(cs.getRoutings()));
            sd.setContainersList(syncEntityConversionService.containersV1ToV2(cs.getContainersList()));
            sd.setShipmentAddresses(syncEntityConversionService.addressesV1ToV2(cs.getShipmentAddresses()));
            sd.setReferenceNumbersList(convertToList(cs.getReferenceNumbers(), ReferenceNumbers.class));
            Map<UUID, String> map = new HashMap<>();
            if(cs.getPackings_() != null)
                map = cs.getPackings_().stream().filter(x-> x.getContainerNumber() != null).collect(Collectors.toMap(PackingRequestV2::getGuid, PackingRequestV2::getContainerNumber));
            sd.setPackingList(syncEntityConversionService.packingsV1ToV2(cs.getPackings_()));
            sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

            sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));
            sd.setGoodsDescription(cs.getDescription());
            
            List<NotesRequest> customerBookingNotes = convertToList(cs.getCustomerBookingNotesList(), NotesRequest.class);
            return shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.
                    buildRequest(modelMapper.map(sd, ShipmentRequest.class)), map, customerBookingNotes, dataMigration, cs.getChangeLogs(), cs.getCreatedBy());
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
        Set<ConsolidationDetails> req = new HashSet<>();
        request.getConsolidationGuids().forEach((key, value) -> {
            ConsolidationDetails p = new ConsolidationDetails();
            p.setGuid(key);
            req.add(p);
        });
        response.setConsolidationList(req);
    }

    private void mapTruckDriverDetailReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(cs.getTruckDriverDetail() == null)
            return;

        List<TruckDriverDetails> req = cs.getTruckDriverDetail().stream()
                .map(item -> {
                    TruckDriverDetails t;
                    t = modelMapper.map(item, TruckDriverDetails.class);
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
        additionalDetails.setNotifyParty(mapPartyObjectWithFreetext(cs.getNotifyParty(), cs.getIsNotifyPartyFreeTextAddress(), cs.getNotifyPartyFreeTextAddress()));
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
        additionalDetails.setIGMFileDate(cs.getIgmfileDate());
        additionalDetails.setIGMFileNo(cs.getIgmfileNo());
        additionalDetails.setIGMInwardDate(cs.getIgminwardDate());
        additionalDetails.setSMTPIGMDate(cs.getSmtpigmdate());
        additionalDetails.setSMTPIGMNumber(cs.getSmtpigmnumber());
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
        additionalDetails.setCustomDeclType(cs.getCustom_DeclType());
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

    private Parties mapPartyObjectWithFreetext(PartyRequestV2 sourcePartyObject, Boolean isFreeText, String freeTextAddress) {
        if(sourcePartyObject == null)
            return null;
        Parties parties = modelMapper.map(sourcePartyObject, Parties.class);
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
