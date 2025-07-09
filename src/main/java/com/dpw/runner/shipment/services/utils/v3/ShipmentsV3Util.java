package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@Component
@Slf4j
public class ShipmentsV3Util {

    private IShipmentSettingsDao shipmentSettingsDao;
    private GetNextNumberHelper getNextNumberHelper;
    private IV1Service v1Service;
    private ProductIdentifierUtility productEngine;
    private JsonHelper jsonHelper;
    private IShipmentDao shipmentDao;
    private CommonUtils commonUtils;
    private MasterDataUtils masterDataUtils;
    private ConsolidationDao consolidationDetailsDao;
    private IRoutingsDao routingsDao;
    private IAwbDao awbDao;
    private IEventDao eventDao;
    private IAuditLogService auditLogService;
    private IPackingDao packingDao;
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    private IReferenceNumbersDao referenceNumbersDao;
    private INotesDao notesDao;
    private IPartiesDao partiesDao;
    private IServiceDetailsDao serviceDetailsDao;
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private EventsV3Util eventsV3Util;
    private IEventsV3Service eventsV3Service;
    private IDateTimeChangeLogService dateTimeChangeLogService;

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Autowired
    ExecutorService executorService;

    @Autowired
    public ShipmentsV3Util(IShipmentSettingsDao shipmentSettingsDao, GetNextNumberHelper getNextNumberHelper,
                           IV1Service v1Service, ProductIdentifierUtility productEngine,
                           IShipmentDao shipmentDao, JsonHelper jsonHelper,
                           CommonUtils commonUtils,
                           MasterDataUtils masterDataUtils,
                           ConsolidationDao consolidationDetailsDao, IRoutingsDao routingsDao, IAwbDao awbDao,
                           IEventDao eventDao, IAuditLogService auditLogService,
                           IPackingDao packingDao, ITruckDriverDetailsDao truckDriverDetailsDao,
                           IReferenceNumbersDao referenceNumbersDao, INotesDao notesDao,
                           IPartiesDao partiesDao, IServiceDetailsDao serviceDetailsDao, IPickupDeliveryDetailsDao pickupDeliveryDetailsDao,
                           EventsV3Util eventsV3Util, IEventsV3Service eventsV3Service,
                           IDateTimeChangeLogService dateTimeChangeLogService
    ) {
        this.shipmentSettingsDao = shipmentSettingsDao;
        this.getNextNumberHelper = getNextNumberHelper;
        this.v1Service = v1Service;
        this.productEngine = productEngine;
        this.shipmentDao = shipmentDao;
        this.jsonHelper = jsonHelper;
        this.commonUtils = commonUtils;
        this.masterDataUtils =masterDataUtils;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.routingsDao = routingsDao;
        this.awbDao = awbDao;
        this.eventDao = eventDao;
        this.auditLogService = auditLogService;
        this.packingDao = packingDao;
        this.truckDriverDetailsDao = truckDriverDetailsDao;
        this.referenceNumbersDao = referenceNumbersDao;
        this.notesDao = notesDao;
        this.partiesDao = partiesDao;
        this.serviceDetailsDao = serviceDetailsDao;
        this.pickupDeliveryDetailsDao = pickupDeliveryDetailsDao;
        this.eventsV3Util = eventsV3Util;
        this.eventsV3Service = eventsV3Service;
        this.dateTimeChangeLogService = dateTimeChangeLogService;

    }

    public String generateShipmentId(ShipmentDetails shipmentDetails) {
        Optional<ShipmentSettingsDetails> shipmentSettingsOptional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
        String shipmentId = "";
        boolean flag = true;
        int counter = 1;
        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                log.info("CR-ID {} || Inside generateShipmentId: with shipmentID: {} | counter: {}", LoggerHelper.getRequestIdFromMDC(), shipmentId, counter++);
                if(shipmentSettingsOptional.isPresent() && Boolean.TRUE.equals(shipmentSettingsOptional.get().getCustomisedSequence())) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(ProductProcessTypes.ShipmentNumber, shipmentDetails);
                    } catch (Exception ignored) {
                        log.error("Exception during common sequence {}", ignored.getMessage());
                        log.error("Exception occurred for common sequence {}", ignored.getStackTrace());
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                ShipmentSettingsDetails shipmentSettings = shipmentSettingsOptional.orElse(null);
                shipmentId = getShipmentId(shipmentId, shipmentSettings);
            }
        }
        return shipmentId;
    }

    private String getCustomizedShipmentProcessNumber(ProductProcessTypes productProcessType, ShipmentDetails currentShipment) throws RunnerException {
        List<TenantProducts> tenantProducts = productEngine.populateEnabledTenantProducts();
        // to check the commmon sequence
        var sequenceNumber = productEngine.getCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.identifyProduct(currentShipment, tenantProducts);
        if (identifiedProduct == null) {
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null) {
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType, tenantProducts);
            if (sequenceSettings == null) {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct(tenantProducts);
                if (defaultProduct == null || identifiedProduct == defaultProduct) {
                    return "";
                }
                sequenceSettings = getNextNumberHelper.getProductSequence(defaultProduct.getId(), productProcessType);
                if (sequenceSettings == null) {
                    return "";
                }
            }
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.TenantId, true, null, false);
    }

    private String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequence table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
    }

    private String getShipmentId(String shipmentId, ShipmentSettingsDetails shipmentSettings) {
        if(StringUtility.isEmpty(shipmentId)) {
            if(shipmentSettings != null) {
                log.info("CR-ID {} || no common sequence found and shipment settings data is: {}",
                        LoggerHelper.getRequestIdFromMDC(),
                        jsonHelper.convertToJson(shipmentSettings));
            }
            log.info("CR-ID {} || no common sequence found", LoggerHelper.getRequestIdFromMDC());
            shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
        }
        return shipmentId;
    }


    public void processVoyageAndFlightNumber(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getCarrierDetails() != null) {
            if (shipmentDetails.getTransportMode() != null && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setVoyage(null);
            } else {
                shipmentDetails.getCarrierDetails().setFlightNumber(null);
            }
        }
    }

    private Long getConsolidationIdFromShipment(ShipmentDetails shipmentDetails) {
        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty())
            consolidationId = shipmentDetails.getConsolidationList().iterator().next().getId();
        return consolidationId;
    }

    private void updateAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) throws RunnerException {
        if(checkForAwbUpdate(shipmentDetails, oldEntity)) {
            awbDao.updatedAwbInformationEvent(shipmentDetails, oldEntity);
        }
    }

    private boolean checkForAwbUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(shipmentDetails.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci())) return true;
        if(!Objects.equals(shipmentDetails.getSecurityStatus(), oldEntity.getSecurityStatus())) return true;
        return !Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), oldEntity.getAdditionalDetails().getEfreightStatus());
    }

    private void storeMblAudit(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (StringUtils.isNotBlank(shipmentDetails.getMasterBill())) {
            List<ConsolidationDetailsProjection> consolidations = consolidationDetailsDao.findMblNumberInDifferentTenant(shipmentDetails.getMasterBill());

            consolidations.forEach(consolidation -> {
                try {
                    if( ObjectUtils.isEmpty(oldEntity) || ObjectUtils.notEqual(oldEntity.getMasterBill(), shipmentDetails.getMasterBill())) {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(MblDuplicatedLog.builder()
                                                .tenantId(consolidation.getTenantId())
                                                .consolidationNo(consolidation.getConsolidationNumber())
                                                .mblNumber(shipmentDetails.getMasterBill())
                                                .shipmentId(shipmentDetails.getShipmentId()).build())
                                        .prevData(null)
                                        .parent(ShipmentDetails.class.getSimpleName())
                                        .parentId(shipmentDetails.getId())
                                        .entityType(MblDuplicatedLog.class.getSimpleName())
                                        .operation(DBOperationType.LOG.name()).build()
                        );
                    }
                } catch (Exception e) {
                    log.error("Unable to store mbl check audit for shipment id: " + shipmentDetails.getId());
                }

            });
        }
    }



    public void afterSaveforEt(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentEtV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean includeGuid) throws RunnerException {
        log.info("shipment afterSave start.... ");
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingV3Request> packingRequestList = shipmentRequest.getPackingList();

        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();

        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();

        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();

        List<RoutingsRequest> routingsRequestList = jsonHelper.convertValueToList(shipmentDetails.getRoutingsList(), RoutingsRequest.class);

        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();

        List<PartiesRequest> shipmentAddressList = shipmentRequest.getShipmentAddresses();

        List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequests = shipmentRequest.getPickupDeliveryDetailsInstructions();

        log.info("shipment afterSave request build.... ");

        storeMblAudit(shipmentDetails, oldEntity);
        log.info("shipment afterSave mblcheck.... ");

        Long id = shipmentDetails.getId();
        Long consolidationId = getConsolidationIdFromShipment(shipmentDetails);

        List<Long> deleteContainerIds = new ArrayList<>();

        if (!isCreate){
            updateAwb(shipmentDetails, oldEntity);
        }
        log.info("shipment afterSave isCreate .... ");
        shipmentRequest.setId(id);
        dateTimeChangeLogService.createEntryFromShipment(jsonHelper.convertValue(shipmentRequest, ShipmentRequest.class), oldEntity);
        log.info("shipment afterSave dateTimeChangeLogService .... ");
        log.info("shipment afterSave bookingCarriageDao.... ");
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isCreate), id);
            shipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        log.info("shipment afterSave truckDriverDetailsDao.... ");

        log.info("shipment afterSave checkSciForAttachConsole.... ");
        List<Events> eventsList = commonUtils.convertToEntityList(eventsRequestList, Events.class, isCreate);
        processEventsInAfterSave(shipmentDetails, oldEntity, isCreate, shipmentSettingsDetails, eventsList, id);

        getUpdatedPackingList(shipmentDetails, isCreate, includeGuid, packingRequestList, consolidationId, id, deleteContainerIds);
        processListRequests(shipmentDetails, isCreate, referenceNumbersRequestList, id, routingsRequestList, serviceDetailsRequestList, notesRequestList, shipmentAddressList, pickupDeliveryDetailsRequests);

        log.info("shipment afterSave createShipmentRouteInConsole..... ");

        log.info("shipment afterSave consoleShipmentMappingDao.deletePendingStateByShipmentId..... ");
        processSyncV1AndAsyncFunctions(shipmentDetails);
        log.info("shipment afterSave end..... ");
    }

    public void processEventsInAfterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, List<Events> eventsList, Long id) throws RunnerException {
        if (eventsList != null) {
            eventsList = setEventDetails(eventsList, shipmentDetails);
            eventsList = eventsV3Util.createOrUpdateEvents(shipmentDetails, oldEntity, eventsList, isCreate);
            if (eventsList != null) {
                commonUtils.updateEventWithMasterData(eventsList);
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                shipmentDetails.setEventsList(updatedEvents);
                eventsV3Service.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
            }
        }
        log.info("shipment afterSave eventDao.updateEntityFromOtherEntity.... ");

        // create Shipment event on the bases of auto create event flag
        if (isCreate && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate()))
            eventsV3Util.autoGenerateCreateEvent(shipmentDetails);
        log.info("shipment afterSave autoGenerateCreateEvent.... ");

        log.info("shipment afterSave generateEvents.... ");
    }

    public List<Events> setEventDetails(List<Events> eventsList, ShipmentDetails shipmentDetails) {
        if (eventsList != null && !eventsList.isEmpty()) {
            for (Events events : eventsList) {
                events.setShipmentNumber(shipmentDetails.getShipmentId());
            }
        }
        return eventsList;
    }

    private List<PackingV3Request> setPackingDetails(List<PackingV3Request> packingRequests, String transportMode, Long consolidationId) {
        if(packingRequests != null && !packingRequests.isEmpty()) {
            for (PackingV3Request packingRequest : packingRequests) {
                if(!isStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
                    packingRequest.setConsolidationId(consolidationId);
                }
            }
        }
        return packingRequests;
    }

    private void getUpdatedPackingList(ShipmentDetails shipmentDetails, boolean isCreate, boolean includeGuid, List<PackingV3Request> packingRequestList, Long consolidationId, Long id, List<Long> deleteContainerIds) throws RunnerException {
        List<Packing> updatedPackings;
        if (packingRequestList != null) {
            packingRequestList = setPackingDetails(packingRequestList, shipmentDetails.getTransportMode(), consolidationId);
            updatedPackings = packingDao.updateEntityFromShipment(commonUtils.convertToEntityList(packingRequestList, Packing.class, !includeGuid && isCreate), id, deleteContainerIds);
            shipmentDetails.setPackingList(updatedPackings);
        }
        log.info("shipment afterSave packingDao.updateEntityFromShipment..... ");
    }


    private void processListRequests(ShipmentDetails shipmentDetails, boolean isCreate, List<ReferenceNumbersRequest> referenceNumbersRequestList, Long id, List<RoutingsRequest> routingsRequestList, List<ServiceDetailsRequest> serviceDetailsRequestList, List<NotesRequest> notesRequestList, List<PartiesRequest> shipmentAddressList, List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequests) throws RunnerException {
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isCreate), id);
            shipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        log.info("shipment afterSave referenceNumbersDao.updateEntityFromShipment..... ");
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(commonUtils.convertToEntityList(routingsRequestList, Routings.class, isCreate), id);
            shipmentDetails.setRoutingsList(updatedRoutings);
        }
        log.info("shipment afterSave routingsDao.updateEntityFromShipment..... ");
        if (serviceDetailsRequestList != null) {
            List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(serviceDetailsRequestList, ServiceDetails.class, isCreate), id);
            shipmentDetails.setServicesList(updatedServiceDetails);
        }
        log.info("shipment afterSave serviceDetailsDao.updateEntityFromShipment..... ");
        if (notesRequestList != null) {
            List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(notesRequestList, Notes.class, isCreate), id, Constants.SHIPMENT);
            shipmentDetails.setNotesList(updatedNotes);
        }
        log.info("shipment afterSave partiesDao.updateEntityFromOtherEntity..... ");
        if (shipmentAddressList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(shipmentAddressList, Parties.class, isCreate), id, Constants.SHIPMENT_ADDRESSES);
            shipmentDetails.setShipmentAddresses(updatedParties);
        }
        log.info("shipment afterSave partiesDao.updateEntityFromOtherEntity..... ");
        if (pickupDeliveryDetailsRequests != null){
            List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(pickupDeliveryDetailsRequests, PickupDeliveryDetails.class , isCreate) , id);
            shipmentDetails.setPickupDeliveryDetailsInstructions(pickupDeliveryDetailsList);
        }
        log.info("shipment afterSave pickupDeliveryDetailsDao.updateEntityFromShipment..... ");
    }


    private void processSyncV1AndAsyncFunctions(ShipmentDetails shipmentDetails) {
        log.info("shipment afterSave syncShipment..... ");
        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails)), executorService);
    }

}
