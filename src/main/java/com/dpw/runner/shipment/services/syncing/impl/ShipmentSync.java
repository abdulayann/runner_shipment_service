package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.TimeZoneConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.utils.CommonUtils.stringValueOf;
import static com.dpw.runner.shipment.services.utils.DateUtils.convertDateToUserTimeZone;
import static java.util.stream.Collectors.toMap;

@Component
@Slf4j
public class ShipmentSync implements IShipmentSync {

    @Value("${v1service.dataSync.username}")
    private String username;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private V1AuthHelper v1AuthHelper;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private ISyncService syncService;
    @Autowired
    private IConsolidationSync consolidationSync;

    private static final String SHIPMENTS = "Shipments";

    @Override
    public ResponseEntity<IRunnerResponse> sync(ShipmentDetails sd, List<UUID> deletedContGuids, List<NotesRequest> customerBookingNotes, String transactionId, boolean isDirectSync) throws RunnerException {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return ResponseHelper.buildSuccessResponse();

        CustomShipmentSyncRequest cs = createShipmentSyncReq(sd, deletedContGuids, customerBookingNotes);

        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(cs).module(SyncingConstants.SHIPMENT).build());
        if (isDirectSync) { // Not being used as of today so change headers accordingly if used in future
            HttpHeaders httpHeaders = v1AuthHelper.getHeadersForDataSyncFromKafka(sd.getCreatedBy(), sd.getTenantId(), null);
            syncService.callSyncAsync(finalCs, StringUtility.convertToString(sd.getId()), StringUtility.convertToString(sd.getGuid()), SHIPMENTS, httpHeaders);
        }
        else {
            if(!Objects.isNull(sd.getSourceGuid()) && !Objects.equals(sd.getGuid(), sd.getSourceGuid())) // Entity Transfer Shipment
                syncService.pushToKafka(finalCs, StringUtility.convertToString(sd.getId()), StringUtility.convertToString(sd.getGuid()), SHIPMENTS, transactionId, sd.getTenantId(), username, UserContext.getUser().getUsername());
            else
                syncService.pushToKafka(finalCs, StringUtility.convertToString(sd.getId()), StringUtility.convertToString(sd.getGuid()), SHIPMENTS, transactionId, sd.getTenantId(), sd.getCreatedBy(), null);
        }
        return ResponseHelper.buildSuccessResponse(modelMapper.map(cs, CustomShipmentSyncRequest.class));
    }

    private CustomShipmentSyncRequest createShipmentSyncReq(ShipmentDetails sd, List<UUID> deletedContGuids, List<NotesRequest> customerBookingNotes) {
        CustomShipmentSyncRequest cs = modelMapper.map(sd, CustomShipmentSyncRequest.class);
        // First map nested entity that are root level properties in v1
        mapAdditionalDetails(cs, sd);
        mapCarrierDetails(cs, sd);
        mapShipmentServices(cs, sd);

        // Map remaining object so there's no info lost for root -> root properties
        // example Guid
        // assigning root level properties not previously mapped
        mapConsolidationGuids(cs, sd);
        cs.setSourceGuid(sd.getSourceGuid());
        cs.setReferenceNo(sd.getBookingReference());
        cs.setCustom_ShipType(sd.getDirection());
        cs.setContainerType(sd.getShipmentType());
        cs.setStatusString(stringValueOf(sd.getStatus()));
        cs.setSalesAgentId(sd.getSalesAgent());
        cs.setInners(sd.getInnerPacks());
        cs.setInnersUnit(sd.getInnerPackUnit());
        cs.setMarksnNums(sd.getMarksNum());
        cs.setConsolidationReferenceNumber(sd.getConsolRef());
        cs.setChargeable(sd.getChargable());
        cs.setChargableUnit(sd.getChargeableUnit());
        cs.setPacks(sd.getNoOfPacks());

        cs.setLockedByUser(sd.getLockedBy()); // lockedBy also present

        cs.setFinanceClosedByUser(sd.getFinanceClosedBy());

        // Fully auto-mapped entities
        // Events, jobs, referenceNumbers, docs, elDetails, services, notes
        // packing (except OriginName field)

        //Mapping root party objects that are not auto-mapped
        cs.setConsignerParty(mapPartyObject(sd.getConsigner()));
        cs.setConsigneeParty(mapPartyObject(sd.getConsignee()));

        // assigning child entities not automatically mapped
        // entityID also gets assigned as a part of this mapping
        mapTruckDriverDetail(cs, sd);
        cs.setRoutings(syncEntityConversionService.routingsV2ToV1(sd.getRoutingsList()));
        mapEvents(cs, sd);
        cs.setContainersList(syncEntityConversionService.containersV2ToV1(sd.getContainersList() != null ? new ArrayList<>(sd.getContainersList()) : null));
        cs.setReferenceNumbers(convertToList(sd.getReferenceNumbersList(), ReferenceNumbersRequestV2.class));
        cs.setPackings_(syncEntityConversionService.packingsV2ToV1(sd.getPackingList(), sd.getContainersList() != null ? new ArrayList<>(sd.getContainersList()) : null, sd.getGuid(), null));
        cs.setShipmentAddresses(syncEntityConversionService.addressesV2ToV1(sd.getShipmentAddresses()));
        cs.setELDetails(convertToList(sd.getElDetailsList(), ElDetailsRequestV2.class));
        cs.setCustomerBookingNotesList(convertToList(customerBookingNotes, NoteRequestV2.class));
        // PickupAddressJSON and DeliveryAddressJSON (could be renamed for easy mapping)

        cs.setBookingCarriages(convertToList(sd.getBookingCarriagesList(), BookingCarriageRequestV2.class));
        cs.setShipmentId(sd.getShipmentId());
        cs.setGuid(sd.getGuid());
        cs.setDescription(sd.getGoodsDescription());
        cs.setCreatedBy(sd.getCreatedBy());
        cs.setClientCountryFilter(sd.getClientCountry());
        cs.setConsigneeCountryFilter(sd.getConsigneeCountry());
        cs.setConsignorCountryFilter(sd.getConsignorCountry());
        cs.setNotifyPartyCountryFilter(sd.getNotifyPartyCountry());
        cs.setCreatedDate(sd.getShipmentCreatedOn());
        
        // Manually mapped fields
        cs.setVolumeWeight(sd.getVolumetricWeight());
        cs.setWeightVolumeUnit(sd.getVolumetricWeightUnit());
        if(sd.getConsignee() != null && Boolean.TRUE.equals(sd.getConsignee().getIsAddressFreeText())){
            cs.setIsConsigneeFreeTextAddress(true);

            var rawData = sd.getConsignee().getAddressData() != null ? sd.getConsignee().getAddressData().get(PartiesConstants.RAW_DATA): null;
            if(rawData!=null)
                cs.setConsigneeFreeTextAddress(rawData.toString());
        }
        else cs.setIsConsigneeFreeTextAddress(false);

        if(sd.getConsigner() != null && Boolean.TRUE.equals(sd.getConsigner().getIsAddressFreeText())){
            cs.setIsConsignerFreeTextAddress(true);
            var rawData = sd.getConsigner().getAddressData() != null ? sd.getConsigner().getAddressData().get(PartiesConstants.RAW_DATA): null;
            if(rawData!=null)
                cs.setConsignerFreeTextAddress(rawData.toString());
        }
        else  cs.setIsConsignerFreeTextAddress(false);

        if(sd.getAdditionalDetails() != null && sd.getAdditionalDetails().getNotifyParty() != null && Boolean.TRUE.equals(sd.getAdditionalDetails().getNotifyParty().getIsAddressFreeText())){
            cs.setIsNotifyPartyFreeTextAddress(true);
            var rawData = sd.getAdditionalDetails().getNotifyParty().getAddressData() != null ? sd.getAdditionalDetails().getNotifyParty().getAddressData().get(PartiesConstants.RAW_DATA): null;
            if(rawData!=null)
                cs.setNotifyPartyFreeTextAddress(rawData.toString());
        }
        else cs.setIsNotifyPartyFreeTextAddress(false);

        cs.setDeletedContGuids(deletedContGuids);
        cs.setOrderNumber(sd.getOrderManagementId());
        cs.setOrderManagementNumber(sd.getOrderManagementNumber());
        return cs;
    }
    @Override
    public void syncLockStatus(ShipmentDetails shipmentDetails) {
        LockSyncRequest lockSyncRequest = LockSyncRequest.builder().guid(shipmentDetails.getGuid()).lockStatus(shipmentDetails.getIsLocked()).build();
        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(lockSyncRequest).module(SyncingConstants.SHIPMENT_LOCK).build());
        syncService.pushToKafka(finalCs, StringUtility.convertToString(shipmentDetails.getId()), StringUtility.convertToString(shipmentDetails.getGuid()), "Shipment Lock Sync", StringUtility.convertToString(shipmentDetails.getGuid()));
    }
    @Override
    public ResponseEntity<IRunnerResponse> syncFromBooking(ShipmentDetails sd, List<UUID> deletedContGuids, List<NotesRequest> customerBookingNotes) throws RunnerException {
        CustomShipmentSyncRequest cs = createShipmentSyncReq(sd, deletedContGuids, customerBookingNotes);
        HttpHeaders httpHeaders = v1AuthHelper.getHeadersForDataSyncFromKafka(sd.getCreatedBy(), sd.getTenantId(), null);
        String shipment = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(cs).module(SyncingConstants.SHIPMENT).build());
        if (!Objects.isNull(sd.getConsolidationList()) && !sd.getConsolidationList().isEmpty()) {
            var console = consolidationDetailsDao.findById(sd.getConsolidationList().iterator().next().getId());
            if (console.isPresent()) {
                CustomConsolidationRequest response = consolidationSync.createConsoleSyncReq(console.get());
                String consolidationRequest = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(response).module(SyncingConstants.CONSOLIDATION).build());
                syncService.callSyncAsync(Arrays.asList(shipment, consolidationRequest) ,
                        Arrays.asList(StringUtility.convertToString(sd.getId()), StringUtility.convertToString(console.get().getId())),
                        Arrays.asList(StringUtility.convertToString(sd.getGuid()), StringUtility.convertToString(console.get().getGuid())),
                        Arrays.asList(SyncingConstants.SHIPMENT, SyncingConstants.CONSOLIDATION), httpHeaders);
            }
            else
                syncService.callSyncAsync(shipment, StringUtility.convertToString(sd.getId()), StringUtility.convertToString(sd.getGuid()), SyncingConstants.SHIPMENT, httpHeaders);
        } else
            syncService.callSyncAsync(shipment, StringUtility.convertToString(sd.getId()), StringUtility.convertToString(sd.getGuid()), SyncingConstants.SHIPMENT, httpHeaders);

        return ResponseHelper.buildSuccessResponse(modelMapper.map(cs, CustomShipmentSyncRequest.class));
    }

    private void mapConsolidationGuids(CustomShipmentSyncRequest response, ShipmentDetails request) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(request.getId());
        response.setConsolidationGuids(new HashMap<>());
        consoleShipmentMappings.forEach(mapping -> {
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(mapping.getConsolidationId());
            response.getConsolidationGuids().put(consolidationDetails.getGuid(), consolidationDetails.getTenantId());
        });
    }

    private PartyRequestV2 mapPartyObject(Parties sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, PartyRequestV2.class);
    }

    private void mapTruckDriverDetail(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getTruckDriverDetails() == null)
            return;
        UUID consolGuid = null;
        Map<Long, UUID> map = new HashMap<>();
        if(sd.getContainersList() != null && !sd.getContainersList().isEmpty())
            map = sd.getContainersList().stream().collect(toMap(Containers::getId, Containers::getGuid));
        if(cs.getConsolidationGuids() != null && !cs.getConsolidationGuids().isEmpty())
            consolGuid = cs.getConsolidationGuids().entrySet().iterator().next().getKey();
        UUID finalConsolGuid = consolGuid;
        Map<Long, UUID> finalMap = map;
        List<TruckDriverDetailsRequestV2> req = sd.getTruckDriverDetails().stream()
                .map(item -> {
                    TruckDriverDetailsRequestV2 t;
                    t = modelMapper.map(item, TruckDriverDetailsRequestV2.class);
                    if(item.getTransporterType() != null)
                        t.setTransporterTypeString(StringUtility.convertToString(item.getTransporterType()));
                    t.setConsolidationGuid(finalConsolGuid);
                    t.setShipmentGuid(sd.getGuid());
                    if(item.getContainerId() != null && finalMap.containsKey(item.getContainerId()))
                        t.setContainerGuid(finalMap.get(item.getContainerId()));
                    return t;
                })
                .toList();

        cs.setTruckDriverDetail(req);

    }

    private void mapCarrierDetails(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getCarrierDetails() == null)
            return;
        modelMapper.map(sd.getCarrierDetails(), cs);
        cs.setDestinationName(sd.getCarrierDetails().getDestination());
        cs.setDestinationPortName(sd.getCarrierDetails().getDestinationPort());
        cs.setOriginName(sd.getCarrierDetails().getOrigin());
        cs.setOriginPortName(sd.getCarrierDetails().getOriginPort());

    }

    private void mapAdditionalDetails(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getAdditionalDetails() == null)
            return;
        modelMapper.map(sd.getAdditionalDetails(), cs);
        if(cs.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            cs.setIssueDate(sd.getAdditionalDetails().getDateOfIssue());
        else
            cs.setDateofIssue(sd.getAdditionalDetails().getDateOfIssue());
        cs.setDateofReceipt(sd.getAdditionalDetails().getDateOfReceipt());
        cs.setReceivingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getReceivingForwarder()));
        cs.setSendingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getSendingForwarder()));
        cs.setTraderOrSupplierParty(mapPartyObject(sd.getAdditionalDetails().getTraderOrSupplier()));
        if(sd.getAdditionalDetails().getAndesStatus() != null)
            cs.setAndesStatusString(String.valueOf(sd.getAdditionalDetails().getAndesStatus().getValue()));
        if(sd.getAdditionalDetails().getOwnership() != null) {
            cs.setOwnershipString(String.valueOf(sd.getAdditionalDetails().getOwnership().getValue()));
            if(sd.getAdditionalDetails().getOwnership().equals(Ownership.SELF))
                cs.setOwnershipName(sd.getAdditionalDetails().getOwnershipName());
            else
                cs.setOwnershipParty(mapPartyObject(sd.getAdditionalDetails().getOwnershipOrg()));
        }
        if(sd.getAdditionalDetails().getPassedBy() != null)
            cs.setPassedByString(String.valueOf(sd.getAdditionalDetails().getPassedBy().getValue()));
        cs.setBoedate(convertDateToUserTimeZone(sd.getAdditionalDetails().getBOEDate(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false));
        cs.setBoenumber(sd.getAdditionalDetails().getBOENumber());
        cs.setIgmfileDate(convertDateToUserTimeZone(sd.getAdditionalDetails().getIGMFileDate(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false));
        cs.setIgmfileNo(sd.getAdditionalDetails().getIGMFileNo());
        cs.setIgminwardDate(convertDateToUserTimeZone(sd.getAdditionalDetails().getIGMInwardDate(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false));
        cs.setSmtpigmdate(convertDateToUserTimeZone(sd.getAdditionalDetails().getSMTPIGMDate(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false));
        cs.setSmtpigmnumber(sd.getAdditionalDetails().getSMTPIGMNumber());
        cs.setHblDeliveryMode(sd.getAdditionalDetails().getDeliveryMode());
        cs.setChargesApply(sd.getAdditionalDetails().getBLChargesDisplay());
        cs.setExporterStmt(sd.getAdditionalDetails().getBLExporterShipment());
        cs.setPlaceOfIssueName(sd.getAdditionalDetails().getPlaceOfIssue());
        cs.setPlaceOfSupplyName(sd.getAdditionalDetails().getPlaceOfSupply());
        cs.setPaidPlaceName(sd.getAdditionalDetails().getPaidPlace());
        cs.setCIFValue(sd.getAdditionalDetails().getCIFValue());
        cs.setCustom_DeclType(sd.getAdditionalDetails().getCustomDeclType());
    }

    private void mapShipmentServices(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getServicesList() == null)
            return;
        List<ShipmentServiceRequestV2> res = sd.getServicesList().stream().map(
                i -> {
                    var _service = modelMapper.map(i, ShipmentServiceRequestV2.class);
                    _service.setServiceDurationSpan(i.getServiceDuration());
                    return _service;
                }
        ).toList();
        cs.setServicesList(res);
    }

    private void mapEvents(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getEventsList() == null)
            return;
        List<EventsRequestV2> res = sd.getEventsList().stream().filter(Objects::nonNull)
                .map(i -> modelMapper.map(i, EventsRequestV2.class)).toList();
        cs.setEventsList(res);
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
