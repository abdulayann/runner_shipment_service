package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ContainerResult;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.MblDuplicatedLog;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.ProductIdentifierUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.MASS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

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

    public ShipmentSummaryWarningsResponse generateSummaryResponseWarnings(ShipmentDetails shipmentDetails, CargoDetailsResponse packsSummary, CargoDetailsResponse containerSummary) throws RunnerException {
        ShipmentSummaryWarningsResponse.WarningDetail packagesWarning = null;
        ShipmentSummaryWarningsResponse.WarningDetail weightWarning = null;
        ShipmentSummaryWarningsResponse.WarningDetail volumeWarning = null;

        if (packsSummary != null && containerSummary != null) {
            // Packages Warning
            Integer packCountPkg = packsSummary.getNoOfPacks();
            Integer packCountCont = containerSummary.getNoOfPacks();

            if (packCountPkg != null && packCountCont != null && !packCountPkg.equals(packCountCont)) {
                packagesWarning = new ShipmentSummaryWarningsResponse.WarningDetail(
                        true,
                        packCountCont + " " + containerSummary.getPacksUnit(),
                        packCountPkg + " " + packsSummary.getPacksUnit(),
                        Math.abs(packCountCont - packCountPkg) + " " + packsSummary.getPacksUnit()
                );
            }

            // Weight
            weightWarning = generateWarning(
                    packsSummary.getWeight(),
                    packsSummary.getWeightUnit(),
                    containerSummary.getWeight(),
                    containerSummary.getWeightUnit(),
                    MASS,
                    shipmentDetails.getWeightUnit()
            );

            // Volume
            if (packsSummary.getVolume() != null && containerSummary.getVolume() != null) {
                volumeWarning = generateWarning(
                        packsSummary.getVolume(),
                        packsSummary.getVolumeUnit(),
                        containerSummary.getVolume(),
                        containerSummary.getVolumeUnit(),
                        VOLUME,
                        shipmentDetails.getVolumeUnit()
                );
            }
        }

        return ShipmentSummaryWarningsResponse.builder()
                .packagesWarning(packagesWarning)
                .weightWarning(weightWarning)
                .volumeWarning(volumeWarning)
                .build();
    }

    public String resolveUnit(List<String> unitsFromData, String branchDefaultUnit) {
        Set<String> distinctUnits = unitsFromData.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

        return (distinctUnits.size() == 1) ? distinctUnits.iterator().next() : branchDefaultUnit;
    }

    public CargoDetailsResponse buildShipmentResponse(Integer packs, Integer dgPacks, String packsType, String dgPacksType, VolumeWeightChargeable vwOb,
                                                      ContainerResult result, BigDecimal weight, String weightUnit, BigDecimal volume, String volumeUnit) {
        return CargoDetailsResponse.builder()
                .weight(weight)
                .weightUnit(weightUnit)
                .volume(volume)
                .volumeUnit(volumeUnit)
                .noOfPacks((packs != null && packs == 0) ? null : packs)
                .packsUnit(packsType)
                .dgPacks(dgPacks)
                .dgPacksUnit(dgPacksType)
                .containers(result.getContainerCount())
                .teuCount(result.getTeuCount())
                .chargable(vwOb.getChargeable())
                .chargeableUnit(vwOb.getChargeableUnit())
                .volumetricWeight(vwOb.getVolumeWeight())
                .volumetricWeightUnit(vwOb.getVolumeWeightUnit())
                .build();
    }

    public ShipmentSummaryWarningsResponse.WarningDetail generateWarning(BigDecimal packVal, String packUnit, BigDecimal contVal, String contUnit, String valueType, String shipmentUnit) throws RunnerException {
        // Convert both container and package values to shipmentUnit before comparison
        BigDecimal convertedContVal = contVal == null ? null
                : new BigDecimal(convertUnit(valueType, contVal, contUnit, shipmentUnit).toString());
        BigDecimal convertedPackVal = packVal == null ? null
                : new BigDecimal(convertUnit(valueType, packVal, packUnit, shipmentUnit).toString());

        boolean mismatch = (convertedPackVal != null && convertedContVal != null && convertedPackVal.compareTo(convertedContVal) != 0);

        String containerDisplay = contVal != null ? contVal.stripTrailingZeros().toPlainString() + " " + contUnit : "";
        String packageDisplay = packVal != null ? packVal.stripTrailingZeros().toPlainString() + " " + packUnit : "";
        String difference = (mismatch)
                ? convertedPackVal.subtract(convertedContVal).abs().stripTrailingZeros().toPlainString() + " " + shipmentUnit
                : null;

        return mismatch
                ? new ShipmentSummaryWarningsResponse.WarningDetail(true, containerDisplay, packageDisplay, difference)
                : null; // No warning
    }

    public List<String> getPacksType(List<Packing> packingList) {
        List<String> packsUnit = new ArrayList<>();
        List<String> dgPacksUnit = new ArrayList<>();
        packingList.forEach(packing -> {
            packsUnit.add(packing.getPacksType());
            if (Boolean.TRUE.equals(packing.getHazardous())) {
                dgPacksUnit.add(packing.getPacksType());
            }
        });
        List<String> packsTypeList = new ArrayList<>();
        packsTypeList.add(resolveUnit(packsUnit, PackingConstants.PKG));
        packsTypeList.add(resolveUnit(dgPacksUnit, PackingConstants.PKG));
        return packsTypeList;
    }

    public List<String> getPacksType(Set<Containers> containersSet) {
        List<String> packsUnit = new ArrayList<>();
        List<String> dgPacksUnit = new ArrayList<>();
        containersSet.forEach(containers -> {
            packsUnit.add(containers.getPacksType());
            if (Boolean.TRUE.equals(containers.getHazardous())) {
                dgPacksUnit.add(containers.getPacksType());
            }
        });
        List<String> packsTypeList = new ArrayList<>();
        packsTypeList.add(resolveUnit(packsUnit, PackingConstants.PKG));
        packsTypeList.add(resolveUnit(dgPacksUnit, PackingConstants.PKG));
        return packsTypeList;
    }

    public ContainerResult getContainerResult(Set<Containers> containersSet) {
        if (CommonUtils.setIsNullOrEmpty(containersSet))
            return new ContainerResult();
        int containerCount = 0;
        int dgContCount = 0;
        BigDecimal teuCount = BigDecimal.ZERO;
        for(Containers containers: containersSet) {
            containerCount += Math.toIntExact(containers.getContainerCount());
            if(Objects.nonNull(containers.getTeu()))
                teuCount = teuCount.add(containers.getTeu());
            if(Boolean.TRUE.equals(containers.getHazardous()))
                dgContCount++;
        }
        return new ContainerResult(containerCount, dgContCount, teuCount);
    }

    public ContainerResult calculateWeightFromContainersFallBack(Set<Containers> containers, String weightUnit) throws RunnerException {
        int containerCount = 0;
        int dgContainerCount = 0;
        BigDecimal teus = BigDecimal.ZERO;
        BigDecimal totalWeight = BigDecimal.ZERO;
        for (Containers c : containers) {
            containerCount += Math.toIntExact(c.getContainerCount());
            if (Boolean.TRUE.equals(c.getHazardous())) dgContainerCount += Math.toIntExact(c.getContainerCount());
            if (Objects.nonNull(c.getTeu())) teus = teus.add(c.getTeu());
            BigDecimal containerWeight = Optional.ofNullable(c.getGrossWeight()).orElse(BigDecimal.ZERO);
            totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, containerWeight, c.getGrossWeightUnit(), weightUnit).toString()));
        }
        return new ContainerResult(containerCount, dgContainerCount, teus, totalWeight);
    }

}
