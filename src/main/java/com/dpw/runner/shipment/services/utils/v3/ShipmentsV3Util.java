package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.ContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.service.impl.ContainerService;
import com.dpw.runner.shipment.services.service.impl.DpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ACTUAL;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus.SAILED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
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
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private CommonUtils commonUtils;
    private MasterDataUtils masterDataUtils;
    private DpsEventService dpsEventService;
    private ShipmentValidationV3Util shipmentValidationV3Util;
    private ContainerService containerService;
    private ContainerDao containerDao;
    private ConsolidationDao consolidationDetailsDao;
    private IConsolidationV3Service consolidationService;
    private IRoutingsDao routingsDao;
    private IAwbDao awbDao;
    private IHblDao hblDao;
    private IEventDao eventDao;
    private IAuditLogService auditLogService;
    private IPackingService packingService;
    private IPackingDao packingDao;
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    private IReferenceNumbersDao referenceNumbersDao;
    private INotesDao notesDao;
    private IPartiesDao partiesDao;
    private IServiceDetailsDao serviceDetailsDao;
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private EventsV3Util eventsV3Util;
    private IEventsV3Service eventsV3Service;

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Autowired
    ExecutorService executorService;

    @Autowired
    public ShipmentsV3Util(IShipmentSettingsDao shipmentSettingsDao, GetNextNumberHelper getNextNumberHelper,
                           IV1Service v1Service, ProductIdentifierUtility productEngine,
                           IShipmentDao shipmentDao, JsonHelper jsonHelper,
                           IConsoleShipmentMappingDao consoleShipmentMappingDao, CommonUtils commonUtils,
                           MasterDataUtils masterDataUtils, DpsEventService dpsEventService,
                           ShipmentValidationV3Util shipmentValidationV3Util, ContainerService containerService,
                           ContainerDao containerDao, ConsolidationDao consolidationDetailsDao,
                           IConsolidationV3Service consolidationService, IRoutingsDao routingsDao, IAwbDao awbDao,
                           IHblDao hblDao, IEventDao eventDao, IAuditLogService auditLogService, IPackingService packingService,
                           IPackingDao packingDao, ITruckDriverDetailsDao truckDriverDetailsDao,
                           IReferenceNumbersDao referenceNumbersDao, INotesDao notesDao,
                           IPartiesDao partiesDao, IServiceDetailsDao serviceDetailsDao, IPickupDeliveryDetailsDao pickupDeliveryDetailsDao,
                           EventsV3Util eventsV3Util, IEventsV3Service eventsV3Service
    ) {
        this.shipmentSettingsDao = shipmentSettingsDao;
        this.getNextNumberHelper = getNextNumberHelper;
        this.v1Service = v1Service;
        this.productEngine = productEngine;
        this.shipmentDao = shipmentDao;
        this.jsonHelper = jsonHelper;
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.commonUtils = commonUtils;
        this.masterDataUtils =masterDataUtils;
        this.dpsEventService = dpsEventService;
        this.shipmentValidationV3Util = shipmentValidationV3Util;
        this.containerService = containerService;
        this.containerDao = containerDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.consolidationService = consolidationService;
        this.routingsDao = routingsDao;
        this.awbDao = awbDao;
        this.hblDao = hblDao;
        this.eventDao = eventDao;
        this.auditLogService = auditLogService;
        this.packingService = packingService;
        this.packingDao = packingDao;
        this.truckDriverDetailsDao = truckDriverDetailsDao;
        this.referenceNumbersDao = referenceNumbersDao;
        this.notesDao = notesDao;
        this.partiesDao = partiesDao;
        this.serviceDetailsDao = serviceDetailsDao;
        this.pickupDeliveryDetailsDao = pickupDeliveryDetailsDao;
        this.eventsV3Util = eventsV3Util;
        this.eventsV3Service = eventsV3Service;

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

    private List<Long> processConsolidationDetailsRequests(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Set<ConsolidationDetailsRequest> consolidationDetailsRequests, List<Long> tempConsolIds) {
        if (consolidationDetailsRequests != null) {
            Set<Long> oldConsolIds = getOldConsolIds(oldEntity, consolidationDetailsRequests, tempConsolIds);
            if (!Objects.isNull(oldConsolIds)) {
                removedConsolIds.addAll(oldConsolIds);
            }

            // Check if the consolidation details are not empty and if one of the following conditions is true:
            // - The old entity is null (no previous data exists).
            // - The old entity's consolidation list is empty (no prior consolidations).
            // - There are removed consolidation IDs (indicating changes in consolidations).
            validateDPSException(shipmentDetails, oldEntity, removedConsolIds, isNewConsolAttached, consolidationDetailsRequests);
        } else {
            shipmentDetails.setConsolRef(null);
            tempConsolIds = Objects.isNull(oldEntity) ? new ArrayList<>() : oldEntity.getConsolidationList().stream().map(e -> e.getId()).toList();
        }
        return tempConsolIds;
    }

    private Set<Long> getOldConsolIds(ShipmentDetails oldEntity, Set<ConsolidationDetailsRequest> consolidationDetailsRequests, List<Long> tempConsolIds) {
        Set<Long> oldConsolIds = Objects.isNull(oldEntity) ? null : oldEntity.getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toSet());
        for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
            if (consolidation.getId() != null) {
                tempConsolIds.add(consolidation.getId());
                if (!Objects.isNull(oldConsolIds) && oldConsolIds.contains(consolidation.getId()))
                    oldConsolIds.remove(consolidation.getId());
            }
        }
        return oldConsolIds;
    }

    private void validateDPSException(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Set<ConsolidationDetailsRequest> consolidationDetailsRequests) {
        if (ObjectUtils.isNotEmpty(consolidationDetailsRequests)
                && (oldEntity == null || ObjectUtils.isEmpty(oldEntity.getConsolidationList()) || ObjectUtils.isNotEmpty(removedConsolIds))) {

            // Check if the specific implication (CONCR) is already present for the given shipment ID.
            // If true, throw a ValidationException to prevent further processing to maintain business constraints.
            if (Objects.nonNull(shipmentDetails.getId()) && Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(shipmentDetails.getId()), DpsConstants.CONCR))) {
                throw new ValidationException(DpsConstants.DPS_ERROR_1);
            }

            isNewConsolAttached.setTrue();
        }
    }

    private Set<ContainerRequest> getContainerRequestsForRemovedConsolIds(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentEtV3Request shipmentRequest, List<Long> removedConsolIds, Set<ContainerRequest> containerRequest) throws RunnerException {
        if (removedConsolIds != null && !removedConsolIds.isEmpty()) {
            shipmentDetails.setConsolRef(null);
            List<Containers> containersList = containerDao.findByConsolidationIdIn(removedConsolIds);
            if (containersList != null && !containersList.isEmpty()) {
                List<Containers> allConsolConts = new ArrayList<>(containersList);
                if (Objects.isNull(containerRequest) && !Objects.isNull(oldEntity)) {
                    containerRequest = new HashSet<>(jsonHelper.convertValueToList(oldEntity.getContainersList().stream().toList(),ContainerRequest.class));
                }
                if (Objects.nonNull(containerRequest)) {
                    containerRequest.removeIf(obj2 -> allConsolConts.stream().anyMatch(obj1 -> obj1.getId().equals(obj2.getId())));
                }
                changeContainerWtVolOnDetach(shipmentRequest, allConsolConts);
            }

        }
        return containerRequest;
    }

    public void changeContainerWtVolOnDetach(ShipmentEtV3Request shipmentRequest, List<Containers> allConsolConts) throws RunnerException {
        Map<Long, List<PackingV3Request>> containerPacksMap = new HashMap<>();
        getContPacksMap(shipmentRequest, containerPacksMap);
        for(Containers container: allConsolConts) {
            if(CARGO_TYPE_FCL.equals(shipmentRequest.getShipmentType())) {
                containerService.changeContainerWtVolForSeaFCLDetach(container);
            } else {
                if(containerPacksMap.containsKey(container.getId())) {
                    List<PackingV3Request> packs = containerPacksMap.get(container.getId());
                    for(PackingV3Request packing : packs) {
                        containerService.changeContainerWtVolForSeaLCLDetach(container, jsonHelper.convertValue(packing, Packing.class));
                    }
                }
            }
        }
        containerDao.saveAll(allConsolConts);
    }

    private void getContPacksMap(ShipmentEtV3Request shipmentRequest, Map<Long, List<PackingV3Request>> containerPacksMap) {
        if(!listIsNullOrEmpty(shipmentRequest.getPackingList())) {
            for(PackingV3Request packing: shipmentRequest.getPackingList()) {
                if(packing.getContainerId() != null) {
                    if(containerPacksMap.containsKey(packing.getContainerId()))
                        containerPacksMap.get(packing.getContainerId()).add(packing);
                    else
                        containerPacksMap.put(packing.getContainerId(), new ArrayList<>(Collections.singletonList(packing)));
                    packing.setContainerId(null);
                }
            }
        }
    }

    private Set<ContainerRequest> getContainerRequestsForAutoWeightVolumeUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<PackingV3Request> packingList, Set<ContainerRequest> containersList) throws RunnerException {
        if (shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate() && packingList != null) {
            if (Objects.isNull(containersList) && !Objects.isNull(oldEntity))
                containersList = new HashSet<>(jsonHelper.convertValueToList(oldEntity.getContainersList().stream().toList(), ContainerRequest.class));
            if(containersList != null && !containersList.isEmpty()) {
                for (ContainerRequest containers : containersList) {
                    List<PackingV3Request> packings = packingList.stream().filter(packing -> Objects.equals(packing.getContainerId(), containers.getId())).toList();
                    BigDecimal totalWeight = BigDecimal.ZERO;
                    BigDecimal totalVolume = BigDecimal.ZERO;
                    processGrossWeightVolumeAndUnits(containers, packings, totalWeight, totalVolume);
                }
            }
        }
        return containersList;
    }

    private void processGrossWeightVolumeAndUnits(ContainerRequest containers, List<PackingV3Request> packings, BigDecimal totalWeight, BigDecimal totalVolume) throws RunnerException {
        if(!packings.isEmpty()) {
            if(isStringNullOrEmpty(containers.getGrossWeightUnit()))
                containers.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
            if(isStringNullOrEmpty(containers.getGrossVolumeUnit()))
                containers.setGrossVolumeUnit(Constants.VOLUME_UNIT_M3);
            for (PackingV3Request packing : packings) {
                if(!isStringNullOrEmpty(packing.getWeightUnit()))
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), containers.getGrossWeightUnit()).toString()));
                if(!isStringNullOrEmpty(packing.getVolumeUnit()))
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), containers.getGrossVolumeUnit()).toString()));
            }
            containers.setGrossWeight(totalWeight);
            containers.setGrossVolume(totalVolume);
        }
    }

    private void callChangeShipmentDGStatusFromPack(Set<Long> dgConts, ShipmentDetails shipmentDetails, PackingV3Request pack, Packing oldPacking) {
        if(Boolean.TRUE.equals(pack.getHazardous())) {
            dgConts.add(pack.getContainerId());
            boolean isDGClass1 = commonUtils.checkIfDGClass1(pack.getDGClass());
            if(pack.getId() == null) {
                commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
                return;
            }
            PackingRequest packingRequest = jsonHelper.convertValue(pack, PackingRequest.class);
            if(oldPacking != null && commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, oldPacking))
                commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
        }
    }

    private void makeDGOceanChangesFromPacksAndContainers(Set<ContainerRequest> containersList, List<PackingV3Request> packingList, ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        Set<Long> dgConts = new HashSet<>();
        Set<Long> newPackAttachedInConts = new HashSet<>();
        Map<Long, Packing> oldPacksMap = new HashMap<>();
        if(!Objects.isNull(oldEntity))
            oldPacksMap = oldEntity.getPackingList().stream().collect(Collectors.toMap(BaseEntity::getId, c -> c));
        if(Objects.isNull(packingList))
            return;
        processPackingList(packingList, shipmentDetails, oldPacksMap, dgConts, newPackAttachedInConts);
        dgConts.remove(null);
        Map<Long, Containers> oldContainersMap = new HashMap<>();
        if(!Objects.isNull(oldEntity))
            oldContainersMap = oldEntity.getContainersList().stream().collect(Collectors.toMap(BaseEntity::getId, c -> c));
        if(Objects.isNull(containersList))
            return;
        for(ContainerRequest container: containersList) {
            if(!Objects.isNull(container.getId()) && dgConts.contains(container.getId())) {
                container.setHazardous(true);
                if(isStringNullOrEmpty(container.getDgClass()) || isStringNullOrEmpty(container.getUnNumber()) || isStringNullOrEmpty(container.getProperShippingName()))
                    throw new ValidationException(OCEAN_DG_CONTAINER_FIELDS_VALIDATION);
            }
            processHazardousContainers(shipmentDetails, container, oldContainersMap, newPackAttachedInConts);
        }
    }

    private void processPackingList(List<PackingV3Request> packingList, ShipmentDetails shipmentDetails, Map<Long, Packing> oldPacksMap, Set<Long> dgConts, Set<Long> newPackAttachedInConts) {
        for(PackingV3Request pack: packingList) {
            Packing oldPacking = null;
            if(oldPacksMap.containsKey(pack.getId()))
                oldPacking = oldPacksMap.get(pack.getId());
            callChangeShipmentDGStatusFromPack(dgConts, shipmentDetails, pack, oldPacking);
            if(!Objects.isNull(pack.getContainerId()) &&
                    (Objects.isNull(oldPacking) || !Objects.equals(pack.getContainerId(), oldPacking.getContainerId())))
                newPackAttachedInConts.add(pack.getContainerId());
        }
    }

    private void processHazardousContainers(ShipmentDetails shipmentDetails, ContainerRequest container, Map<Long, Containers> oldContainersMap, Set<Long> newPackAttachedInConts) {
        if(Boolean.TRUE.equals(container.getHazardous())) {
            Containers oldContainer = null;
            boolean isDGClass1 = commonUtils.checkIfDGClass1(container.getDgClass());
            if(container.getId() == null) {
                commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
            }
            if(container.getId() != null) {
                if(oldContainersMap.containsKey(container.getId()))
                    oldContainer = oldContainersMap.get(container.getId());
                if(oldContainer != null && commonUtils.checkIfDGFieldsChangedInContainer(container, oldContainer)) {
                    commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
                }
                if(newPackAttachedInConts.contains(container.getId()))
                    commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
            }
        }
    }

    private Long getConsolidationIdFromShipment(ShipmentDetails shipmentDetails) {
        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty())
            consolidationId = shipmentDetails.getConsolidationList().iterator().next().getId();
        return consolidationId;
    }

    private Set<Containers> getUpdatedContainers(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, Set<ContainerRequest> containerRequest, Long consolidationId, Long id) throws RunnerException {
        Set<Containers> updatedContainers = new HashSet<>();

        if (containerRequest != null) {
            for (ContainerRequest containerRequest1 : containerRequest) {
                containerRequest1.setConsolidationId(consolidationId);
                if (Boolean.TRUE.equals(containerRequest1.getHazardous()))
                    shipmentDetails.setContainsHazardous(true);
            }
            updatedContainers = new HashSet<>(containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(new ArrayList<>(containerRequest),
                    Containers.class, isCreate), consolidationId, id, false));
        } else if (!Objects.isNull(oldEntity)) {
            updatedContainers = oldEntity.getContainersList();
        }
        return updatedContainers;
    }

    private void addAgentDetailsForConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(shipmentDetails.getAdditionalDetails() != null) {
            consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(shipmentDetails.getAdditionalDetails().getExportBroker()));
            consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(shipmentDetails.getAdditionalDetails().getImportBroker()));
        }
        if (Objects.equals(consolidationDetails.getShipmentType(), DIRECTION_EXP) && CommonUtils.checkAddressNotNull(consolidationDetails.getReceivingAgent())) {
            Long receivingBranchId = commonUtils.getReceivingBranch(consolidationDetails.getReceivingAgent().getOrgId(), consolidationDetails.getReceivingAgent().getAddressId());
            consolidationDetails.setReceivingBranch(receivingBranchId);
        }
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsEntityTransferPrerequisiteEnabled())) {
            if(!commonUtils.checkIfPartyExists(consolidationDetails.getSendingAgent())) {
                consolidationDetails.setSendingAgentCountry(commonUtils.getCountryFromUnLocCode(consolidationDetails.getCarrierDetails().getOriginPortLocCode()));
            }
            if(!commonUtils.checkIfPartyExists(consolidationDetails.getReceivingAgent())) {
                consolidationDetails.setReceivingAgentCountry(commonUtils.getCountryFromUnLocCode(consolidationDetails.getCarrierDetails().getDestinationPortLocCode()));
            }
        }
    }

    private List<Routings> createConsoleRoutePayload(List<Routings> routes){
        List<Routings> responseList = new ArrayList<>();
        for (var route : routes){
            Routings routings = new Routings();
            routings.setLeg(1L);
            routings.setPol(route.getPol());
            routings.setPod(route.getPod());
            routings.setMode(route.getMode());
            routings.setEta(route.getEta());
            routings.setEtd(route.getEtd());
            routings.setTransitDays(route.getTransitDays());
            routings.setAta(route.getAta());
            routings.setAtd(route.getAtd());
            routings.setVesselName(route.getVesselName());
            routings.setVoyage(route.getVoyage());
            routings.setCarrier(route.getCarrier());
            routings.setFlightNumber(route.getFlightNumber());
            responseList.add(routings);
        }
        return responseList;
    }

    private List<Routings> getRoutingsList(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        List<Routings> routings = new ArrayList<>();
        if(shipmentDetails.getRoutingsList() != null && !shipmentDetails.getRoutingsList().isEmpty())
            routings = shipmentDetails.getRoutingsList().stream().sorted(Comparator.comparingLong(Routings::getLeg)).toList();
        var routeRequest = routings.stream().filter(x -> x.getMode().equals(shipmentDetails.getTransportMode())).findFirst();
        List<Routings> createRoutes = new ArrayList<>();
        // Generate default Routes if Route Master is enabled
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            createRoutes.addAll(routingsDao.generateDefaultRouting(consolidationDetails.getCarrierDetails(), shipmentDetails.getTransportMode()));
            consolidationDetails.setRoutingsList(createRoutes);
        }
        else {
            if(routeRequest.isPresent()) {
                createRoutes.add(jsonHelper.convertValue(routeRequest.get(), Routings.class));
                createRoutes = createConsoleRoutePayload(createRoutes);
                consolidationDetails.setRoutingsList(createRoutes);
            }
        }
        return createRoutes;
    }

    private void validateCreateConsolidations(ShipmentDetails shipmentDetails, ShipmentSettingsDetails shipmentSettings) {
        if((shipmentSettings.getConsolidationLite() == null || !shipmentSettings.getConsolidationLite())
                && !Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
                && (StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) || StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()))) {
            throw new ValidationException("Not able to create consolidation, before adding 'New Containers' , please provide ‘Origin’ and ‘Destination’ values.");
        }
        if(StringUtility.isNotEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort())) {
            throw new ValidationException("‘Origin’ and ‘Destination’ can't be same");
        }
    }

    private Events createEvent(ConsolidationDetails consolidationDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from consolidation
        events.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.CONSOLIDATION);
        events.setEntityId(consolidationDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setConsolidationId(consolidationDetails.getId());
        events.setDirection(consolidationDetails.getShipmentType());
        // Persist the event
        eventDao.save(events);
        return events;
    }

    public ConsolidationDetails createConsolidation(ShipmentDetails shipmentDetails, List<Containers> containers) throws RunnerException {
        ShipmentSettingsDetails shipmentSettings = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettings.getShipConsolidationContainerEnabled())) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setConsolidationType(shipmentDetails.getJobType());
            consolidationDetails.setTransportMode(shipmentDetails.getTransportMode());
            validateCreateConsolidations(shipmentDetails, shipmentSettings);
            consolidationDetails.setCarrierDetails(jsonHelper.convertValue(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
            consolidationDetails.getCarrierDetails().setId(null);
            consolidationDetails.getCarrierDetails().setGuid(null);
            if(shipmentSettings.getShipmentLite() != null && shipmentSettings.getShipmentLite() && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getDirection().equals(Constants.DIRECTION_EXP)) {
                consolidationDetails.setPayment(shipmentDetails.getPaymentTerms());
            }
            if(consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setOrigin(consolidationDetails.getCarrierDetails().getOriginPort());
                consolidationDetails.getCarrierDetails().setOriginLocCode(consolidationDetails.getCarrierDetails().getOriginPortLocCode());
                consolidationDetails.getCarrierDetails().setDestination(consolidationDetails.getCarrierDetails().getDestinationPort());
                consolidationDetails.getCarrierDetails().setDestinationLocCode(consolidationDetails.getCarrierDetails().getDestinationPortLocCode());
            }
            consolidationDetails.setShipmentType(shipmentDetails.getDirection());
            consolidationDetails.setContainerCategory(shipmentDetails.getShipmentType());
            consolidationDetails.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetails.setIsSendingAgentFreeTextAddress(false);
            consolidationDetails.setIsInland(false);
            consolidationDetails.setCarrierBookingRef(shipmentDetails.getBookingNumber());
            consolidationDetails.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            consolidationDetails.setDepartment(commonUtils.getAutoPopulateDepartment(
                    shipmentDetails.getTransportMode(), shipmentDetails.getDirection(), MdmConstants.CONSOLIDATION_MODULE
            ));
            if(StringUtility.isNotEmpty(shipmentDetails.getMasterBill())) {
                consolidationDetails.setBol(shipmentDetails.getMasterBill());
            }
            if(Objects.equals(TRANSPORT_MODE_SEA, shipmentDetails.getTransportMode()) || Objects.equals(TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()))
                consolidationDetails.setHazardous(shipmentDetails.getContainsHazardous());
            consolidationService.generateConsolidationNumber(consolidationDetails);
            addAgentDetailsForConsole(shipmentDetails, consolidationDetails);
            List<Routings> createRoutes = getRoutingsList(shipmentDetails, consolidationDetails);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()));
            if(!CommonUtils.listIsNullOrEmpty(createRoutes)) {
                routingsDao.saveEntityFromConsole(createRoutes, consolidationDetails.getId());
            }
            Long id = consolidationDetails.getId();
            containers = getConsoleContainers(containers, id);
            consolidationDetails.setContainersList(containers);
            createConsolidationEvent(shipmentSettings, consolidationDetails);
            consolidationService.pushShipmentDataToDependentService(consolidationDetails, true, null);
            return consolidationDetails;
        }
        return null;
    }

    private List<Containers> getConsoleContainers(List<Containers> containers, Long id) {
        if(containers != null && !containers.isEmpty()) {
            containers = containers.stream().map(e -> e.setConsolidationId(id)).toList();
            containers = containerDao.saveAll(containers);
        }
        return containers;
    }

    private void createConsolidationEvent(ShipmentSettingsDetails shipmentSettings, ConsolidationDetails consolidationDetails) {
        if(shipmentSettings.getAutoEventCreate() != null && shipmentSettings.getAutoEventCreate()) {
            if (consolidationDetails.getEventsList() == null) {
                consolidationDetails.setEventsList(new ArrayList<>());
            }
            consolidationDetails.getEventsList().add(createEvent(consolidationDetails, EventConstants.COCR));
        }
    }

    private boolean getUpdatedSyncConsole(ShipmentDetails shipmentDetails, boolean isCreate, ShipmentEtV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, Set<Containers> updatedContainers, List<Long> tempConsolIds, boolean syncConsole) throws RunnerException {
        ConsolidationDetails consolidationDetails;
        if((!updatedContainers.isEmpty() || (shipmentRequest.getAutoCreateConsole() != null  && shipmentRequest.getAutoCreateConsole())) && (tempConsolIds == null || tempConsolIds.isEmpty()) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {

            // Check if the specific implication (CONCR) is already present for the given shipment ID.
            // If true, throw a ValidationException to prevent further processing.
            if (Objects.nonNull(shipmentDetails.getId()) && Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(shipmentDetails.getId()), DpsConstants.CONCR))) {
                throw new ValidationException(DpsConstants.DPS_ERROR_1);
            }

            if(!isCreate) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipmentDetails.getId(), "=", null);
                listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);
                processConsoleShipmentEmails(shipmentDetails, consoleShipmentMappingsForEmails);
            }
            consolidationDetails = createConsolidation(shipmentDetails, new ArrayList<>(updatedContainers));
            if (!Objects.isNull(consolidationDetails)) {
                shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails)));
                setMasterBillForConsole(shipmentDetails, consolidationDetails);
            }
        }
        return syncConsole;
    }

    private void setMasterBillForConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if (isStringNullOrEmpty(shipmentDetails.getMasterBill()))
            shipmentDetails.setMasterBill(consolidationDetails.getBol());
    }

    private void processConsoleShipmentEmails(ShipmentDetails shipmentDetails, List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails) {
        if(!listIsNullOrEmpty(consoleShipmentMappingsForEmails)) {
            consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
            List<Long> otherConsoleIds = consoleShipmentMappingsForEmails.stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
            List<ConsolidationDetails> otherConsolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(otherConsoleIds));
            commonUtils.sendRejectionEmailsExplicitly(List.of(shipmentDetails), consoleShipmentMappingsForEmails, new HashSet<>(), otherConsolidationDetails);
        }
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    public void dgValidations(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if( ((Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()) && SHIPMENT_TYPE_LCL.equals(consolidationDetails1.getContainerCategory()))
                || checkForAirDGFlag(consolidationDetails1))
                && (Boolean.TRUE.equals(consolidationDetails1.getHazardous()) || Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()))) {
            List<ConsoleShipmentMapping> consoleShipmentMapping = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails1.getId());
            validateDetails(consolidationDetails1, isNewConsoleAttached, consoleShipmentMapping);
        }
    }

    private void validateDetails(ConsolidationDetails consolidationDetails1, int isNewConsoleAttached, List<ConsoleShipmentMapping> consoleShipmentMapping) throws RunnerException {
        if(!listIsNullOrEmpty(consoleShipmentMapping) && consoleShipmentMapping.size() + isNewConsoleAttached > 1) {
            if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()))
                throw new RunnerException("For Ocean DG Consolidation LCL Cargo Type, and can have only 1 shipment");
            else {
                if(isNewConsoleAttached == 1)
                    throw new RunnerException(String.format(CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL, consolidationDetails1.getConsolidationNumber()));
                else
                    throw new RunnerException(CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS);
            }
        }
    }

    private void deletePendingRequestsOnConsoleAttach(ShipmentDetails shipmentDetails, boolean isCreate) {
        if(!isCreate) {
            ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipmentDetails.getId(), "=", null);
            listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);
            processConsoleShipmentEmails(shipmentDetails, consoleShipmentMappingsForEmails);
        }
    }

    private void processIsNewConsolAttached(ShipmentDetails shipmentDetails, boolean isCreate, MutableBoolean isNewConsolAttached) throws RunnerException {
        if (Boolean.TRUE.equals(isNewConsolAttached.getValue())) {
            ConsolidationDetails consolidationDetails1 = shipmentDetails.getConsolidationList().iterator().next();
            List<Routings> routings = routingsDao.findRoutingsByConsolidationId(consolidationDetails1.getId());
            consolidationService.syncMainCarriageRoutingToShipment(routings, shipmentDetails);
            dgValidations(shipmentDetails, consolidationDetails1, 1);
            if (shipmentDetails.getCargoDeliveryDate() != null && consolidationDetails1.getLatDate() != null && consolidationDetails1.getLatDate().isAfter(shipmentDetails.getCargoDeliveryDate())) {
                throw new RunnerException("Cargo Delivery Date is lesser than LAT Date.");
            }
            shipmentDetails.setMasterBill(consolidationDetails1.getBol());
            shipmentDetails.setDirection(consolidationDetails1.getShipmentType());
            if (consolidationDetails1.getId() != null) {
                Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationDetails1.getId());
                if (consol.isPresent() && !CommonUtils.isStringNullOrEmpty(consol.get().getBookingNumber())) {
                    shipmentDetails.setBookingNumber(consol.get().getBookingNumber());
                } else
                    consol.ifPresent(consolidationDetails -> shipmentDetails.setBookingNumber(consolidationDetails.getCarrierBookingRef()));
            }
            processCarrierDetailsForShipmentConsole(shipmentDetails, consolidationDetails1);
            processInterBranchConsoleInBeforeSave(shipmentDetails, consolidationDetails1);
            ConsolidationDetails console = shipmentDetails.getConsolidationList().iterator().next();
            if (!Objects.isNull(console) && !Objects.isNull(console.getId()))
                awbDao.validateAirMessaging(console.getId());
            deletePendingRequestsOnConsoleAttach(shipmentDetails, isCreate);
        }
    }

    private void processCarrierDetailsForShipmentConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1) {
        if (shipmentDetails.getCarrierDetails() == null) {
            shipmentDetails.setCarrierDetails(new CarrierDetails());
        }
        if (consolidationDetails1.getCarrierDetails() != null) {
            shipmentDetails.getCarrierDetails().setVoyage(consolidationDetails1.getCarrierDetails().getVoyage());
            shipmentDetails.getCarrierDetails().setVessel(consolidationDetails1.getCarrierDetails().getVessel());
            shipmentDetails.getCarrierDetails().setShippingLine(consolidationDetails1.getCarrierDetails().getShippingLine());
            shipmentDetails.getCarrierDetails().setAircraftType(consolidationDetails1.getCarrierDetails().getAircraftType());
            shipmentDetails.getCarrierDetails().setCfs(consolidationDetails1.getCarrierDetails().getCfs());

            if (Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setFlightNumber(consolidationDetails1.getCarrierDetails().getFlightNumber());
                shipmentDetails.getCarrierDetails().setOriginPort(consolidationDetails1.getCarrierDetails().getOriginPort());
                shipmentDetails.getCarrierDetails().setDestinationPort(consolidationDetails1.getCarrierDetails().getDestinationPort());
                shipmentDetails.getCarrierDetails().setEtd(consolidationDetails1.getCarrierDetails().getEtd());
                shipmentDetails.getCarrierDetails().setEta(consolidationDetails1.getCarrierDetails().getEta());
                shipmentDetails.getCarrierDetails().setAtd(consolidationDetails1.getCarrierDetails().getAtd());
                shipmentDetails.getCarrierDetails().setAta(consolidationDetails1.getCarrierDetails().getAta());
            }
        }
    }

    private void processInterBranchConsoleInBeforeSave(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
                if(shipmentDetails.getAdditionalDetails() != null &&
                        !CommonUtils.checkSameParties(consolidationDetails.getSendingAgent(), shipmentDetails.getAdditionalDetails().getExportBroker())) {
                    shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
                } else if (shipmentDetails.getAdditionalDetails() == null) {
                    shipmentDetails.setAdditionalDetails(new AdditionalDetails());
                    shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
                }
            }

            if (CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
                if(shipmentDetails.getAdditionalDetails() != null &&
                        !CommonUtils.checkSameParties(consolidationDetails.getReceivingAgent(), shipmentDetails.getAdditionalDetails().getImportBroker())) {
                    shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
                } else if (shipmentDetails.getAdditionalDetails() == null) {
                    shipmentDetails.setAdditionalDetails(new AdditionalDetails());
                    shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
                }
            }
        }
    }

    private void processBranchesAndPartner(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getReceivingBranch() != null && shipmentDetails.getReceivingBranch() == 0) {
            shipmentDetails.setReceivingBranch(null);
        }
        if(shipmentDetails.getReceivingBranch() == null && !setIsNullOrEmpty(shipmentDetails.getConsolidationList()) && Boolean.TRUE.equals(shipmentDetails.getConsolidationList().iterator().next().getInterBranchConsole())){
            shipmentDetails.setReceivingBranch(shipmentDetails.getConsolidationList().iterator().next().getReceivingBranch());
            shipmentDetails.setIsReceivingBranchAdded(false);
        }
        if (ObjectUtils.isNotEmpty(shipmentDetails.getTriangulationPartnerList())
                && shipmentDetails.getTriangulationPartnerList().size() == 1) {
            TriangulationPartner triangulationPartner = shipmentDetails.getTriangulationPartnerList().get(0);
            if (triangulationPartner != null
                    && Long.valueOf(0).equals(triangulationPartner.getTriangulationPartner())) {
                shipmentDetails.setTriangulationPartnerList(null);
            }
        } else if (shipmentDetails.getTriangulationPartnerList() == null
                && shipmentDetails.getTriangulationPartner() != null
                && shipmentDetails.getTriangulationPartner() == 0) {
            shipmentDetails.setTriangulationPartner(null);
        }
        if(shipmentDetails.getDocumentationPartner() != null && shipmentDetails.getDocumentationPartner() == 0)
            shipmentDetails.setDocumentationPartner(null);
    }

    private boolean checkOriginalPrintedForJobTypeChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(oldEntity == null)
            return false;
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA))
            return false;
        if(!Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP))
            return false;
        if(!Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPrintedOriginal()))
            return false;
        return !Objects.equals(shipmentDetails.getJobType(), oldEntity.getJobType());
    }

    private boolean checkDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,ShipmentSettingsDetails shipmentSettingsDetails){
        if(oldEntity == null)
            return false;
        if(!Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag()))
            return false;
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        if(!Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT))
            return false;
        return !Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) || !Objects.equals(shipmentDetails.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort())
                || !Objects.equals(shipmentDetails.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine());
    }

    private void updateAwbForDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        if(checkDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails)) {
            List<Awb> awbs = awbDao.findByShipmentId(shipmentDetails.getId());
            if(!awbs.isEmpty()) {
                Awb awb = awbs.get(0);
                awb.getAwbGoodsDescriptionInfo().forEach(x -> {
                    x.setDisableFetchRates(false);
                    x.setEnableFetchRatesWarning(true);
                });
                awbDao.save(awb);
            }
        }
    }

    public boolean checkIfLCLConsolidationEligible(ShipmentDetails shipmentDetails) {
        if(!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            return false;
        if(!Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()))
            return false;
        if(!Constants.SHIPMENT_TYPE_LCL.equals(shipmentDetails.getShipmentType()))
            return false;
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    private void setShipmentPackStatusOnAssigned(ShipmentDetails shipmentDetails, boolean partialAssigned, boolean fullAssigned, boolean partialGated, boolean fullGated) {
        if(partialAssigned)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIALLY_ASSIGNED);
        if(fullAssigned)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.ASSIGNED);
        if(partialGated)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIAL_CARGO_GATE_IN);
        if(fullGated)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.CARGO_GATED_IN);
    }

    private void processPackingRequests(List<PackingV3Request> packingRequests, ShipmentDetails shipmentDetails) {
        boolean fullGated = true;
        boolean partialGated = false;
        boolean fullAssigned = true;
        boolean partialAssigned = false;
        LocalDateTime maxDate = null;
        for (PackingV3Request packingRequest: packingRequests) {
            if(packingRequest.getCargoGateInDate() != null) {
                if(ACTUAL.equals(packingRequest.getDateType()))
                    partialGated = true;
                else
                    fullGated = false;
                if(maxDate == null || packingRequest.getCargoGateInDate().isAfter(maxDate)) {
                    shipmentDetails.setShipmentGateInDate(packingRequest.getCargoGateInDate());
                    shipmentDetails.setDateType(packingRequest.getDateType());
                    maxDate = packingRequest.getCargoGateInDate();
                }
            }
            else
                fullGated = false;
            if(packingRequest.getContainerId() != null)
                partialAssigned = true;
            else
                fullAssigned = false;
        }
        setShipmentPackStatusOnAssigned(shipmentDetails, partialAssigned, fullAssigned, partialGated, fullGated);
    }

    private void updateShipmentGateInDateAndStatusFromPacks(List<PackingV3Request> packingRequests, ShipmentDetails shipmentDetails) throws RunnerException {
        shipmentDetails.setShipmentPackStatus(null);
        if(packingRequests != null && !packingRequests.isEmpty()) {
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.BOOKED);
            processPackingRequests(packingRequests, shipmentDetails);
        }
        if(shipmentDetails.getCarrierDetails() != null && shipmentDetails.getCarrierDetails().getAtd() != null)
            shipmentDetails.setShipmentPackStatus(SAILED);
        if(shipmentDetails.getShipmentGateInDate() != null) {
            if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()
                    && shipmentDetails.getConsolidationList().iterator().next().getCfsCutOffDate() != null) {
                if(shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getConsolidationList().iterator().next().getCfsCutOffDate()))
                    throw new RunnerException("Shipment Gate In date should not be greater than the CFS Cut Off Date entered at the consolidation level.");
            }
            else if(shipmentDetails.getCarrierDetails().getEtd() != null && shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getCarrierDetails().getEtd()))
                throw new RunnerException("Shipment Gate In Date cannot be greater than ETD.");
        }
    }
    

    @SuppressWarnings("java:S125")
    public void beforeSaveForEt(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentEtV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, boolean isImportFile) throws RunnerException{

        List<Long> tempConsolIds = new ArrayList<>();
        Long id = !Objects.isNull(oldEntity) ? oldEntity.getId() : null;
        boolean syncConsole = false;

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        Set<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();

        tempConsolIds = processConsolidationDetailsRequests(shipmentDetails, oldEntity, removedConsolIds, isNewConsolAttached, consolidationDetailsRequests, tempConsolIds);

        List<PackingV3Request> packingRequest = shipmentRequest.getPackingList();
        Set<ContainerRequest> containerRequest = shipmentRequest.getContainersList();

        containerRequest = getContainerRequestsForRemovedConsolIds(shipmentDetails, oldEntity, shipmentRequest, removedConsolIds, containerRequest);

        containerRequest = getContainerRequestsForAutoWeightVolumeUpdate(shipmentDetails, oldEntity, packingRequest, containerRequest);
        if (Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            makeDGOceanChangesFromPacksAndContainers(containerRequest, packingRequest, shipmentDetails, oldEntity);

        Long consolidationId = getConsolidationIdFromShipment(shipmentDetails);
        Set<Containers> updatedContainers = getUpdatedContainers(shipmentDetails, oldEntity, isCreate, containerRequest, consolidationId, id);
        shipmentDetails.setContainersList(updatedContainers);

        getUpdatedSyncConsole(shipmentDetails, isCreate, shipmentRequest, shipmentSettingsDetails, updatedContainers, tempConsolIds, syncConsole);

        if(Boolean.TRUE.equals(shipmentRequest.getIsChargableEditable())){
            shipmentDetails.setChargable(shipmentRequest.getChargable());
        }
        shipmentValidationV3Util.validateBeforeSaveForEt(shipmentDetails);


        processIsNewConsolAttached(shipmentDetails, isCreate, isNewConsolAttached);

        processBranchesAndPartner(shipmentDetails);

        if(Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDraftPrinted())
                && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentDetails.getId());
            if(!hbls.isEmpty()) {
                hblDao.delete(hbls.get(0));
            }
            shipmentDetails.getAdditionalDetails().setDraftPrinted(false);
        }
        if(checkOriginalPrintedForJobTypeChange(shipmentDetails, oldEntity)){
            throw new ValidationException("Consolidation type cannot be changed as the original BL has been generated for this shipment.");
        }
        updateAwbForDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails);

        if(checkIfLCLConsolidationEligible(shipmentDetails))
            updateShipmentGateInDateAndStatusFromPacks(packingRequest, shipmentDetails);

        var tenantSettings = Optional.ofNullable(commonUtils.getCurrentTenantSettings()).orElse(V1TenantSettingsResponse.builder().build());
        // If TransportModeConfig flag is ON, this block will check for the valid transport mode
        if (Boolean.TRUE.equals(tenantSettings.getTransportModeConfig()) && Boolean.FALSE.equals(isImportFile) && (isCreate || !Objects.equals(oldEntity.getTransportMode(), shipmentDetails.getTransportMode()))
                && Boolean.FALSE.equals(commonUtils.isTransportModeValid(shipmentDetails.getTransportMode(), Constants.SHIPMENT_DETAILS, tenantSettings))) {
            throw new ValidationException(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, shipmentDetails.getTransportMode()));
        }


        // Ignore events payload to avoid transaction issues bypassing shipmentDetailsDao.update(...);
        // Update happens in after save from request body
        shipmentDetails.setEventsList(null);
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



    public void afterSaveforEt(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentEtV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, boolean includeGuid) throws RunnerException {
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
        log.info("shipment afterSave dateTimeChangeLogService .... ");
        log.info("shipment afterSave bookingCarriageDao.... ");
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isCreate), id);
            shipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        log.info("shipment afterSave truckDriverDetailsDao.... ");


        // Sci status update for attach and detach in console mawb
        checkSciForAttachDetachConsole(shipmentDetails, removedConsolIds, isNewConsolAttached, consolidationId);
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

        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(shipmentDetails);
        log.info("shipment afterSave generateEvents.... ");
    }

    @SuppressWarnings({"java:S1066", "java:S2583"})
    public void autoGenerateEvents(ShipmentDetails shipmentDetails) {
        Events response = null;
        if (shipmentDetails.getStatus() != null) {
            // LATER : remove this
            if (response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
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

    private void checkSciForAttachDetachConsole(ShipmentDetails shipmentDetails, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Long consolidationId) throws RunnerException {
        if(removedConsolIds != null && !removedConsolIds.isEmpty() && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)){
            consolidationService.checkSciForDetachConsole(removedConsolIds.get(0));
        }
        log.info("shipment afterSave checkSciForDetachConsole.... ");
        if(Boolean.TRUE.equals(isNewConsolAttached.getValue()) && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            consolidationService.checkSciForAttachConsole(consolidationId);
        }
    }

    public boolean checkAttachDgAirShipments(ConsolidationDetails consolidationDetails){
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return true;
        if(!Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return true;
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return true;
        if(consolidationDetails.getShipmentsList() == null || consolidationDetails.getShipmentsList().isEmpty())
            return false;
        return consolidationDetails.getShipmentsList().stream().anyMatch(ship -> Boolean.TRUE.equals(ship.getContainsHazardous()));
    }

    @SuppressWarnings("java:S3655")
    private ConsolidationDetails makeConsoleNonDg(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, List<Long> removedConsolIds, ConsolidationDetails consolidationDetails) {
        if(!isCreate && removedConsolIds != null && !removedConsolIds.isEmpty()) {
            boolean makeConsoleNonDG = checkForDGShipmentAndAirDgFlag(oldEntity); // check if removed shipment was dg
            if(makeConsoleNonDG) {
                consolidationDetails = consolidationDetailsDao.findById(removedConsolIds.get(0)).get();
                if(!checkAttachDgAirShipments(consolidationDetails)) // check if any other attached shipment is dg
                    changeConsolidationDGValues(false, new AtomicBoolean(true), removedConsolIds.get(0), shipmentDetails, consolidationDetails);
            }
        }
        return consolidationDetails;
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

    private ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Set<ConsolidationDetails> consolidationList, ShipmentDetails shipment) {
        if(consolidationList != null && !consolidationList.isEmpty()) {
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList.iterator().next().getId(), shipment, null);
        }
        return null;
    }


    @SuppressWarnings("java:S3655")
    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentEtV3Request shipmentRequest) throws RunnerException {
        Set<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
        ConsolidationDetails consolidationDetails;
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var linkedConsol = (consolidationList != null && !consolidationList.isEmpty()) ? consolidationList.iterator().next() : null;
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)) {
            consolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId()).get();
            if (Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)) {
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        processPackUtilisationCalculationInConsole(shipment, oldEntity, shipmentRequest, linkedConsol);
        boolean makeConsoleDG = checkForDGShipmentAndAirDgFlag(shipment) || checkForOceanDGShipment(shipment);
        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(checkForNonDGShipmentAndAirDgFlag(shipment));
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(shipment.getAdditionalDetails() != null && Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1));
        if(linkedConsol != null && isDiffPresentInOldNewConsole(shipment, oldEntity)) {
            consolidationDetails = processLinkedConsolidationDetails(shipment, oldEntity, consolidationList, makeConsoleDG, makeConsoleNonDG, makeConsoleSciT1);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, Boolean.TRUE.equals(shipment.getContainsHazardous()));
            return consolidationDetails;
        }
        else // only execute when above logic execution not required (i.e. saving all shipments not required)
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList, shipment);
    }

    public ConsolidationDetails getConsolidationDetails(Long consolidationId, ConsolidationDetails consolidationDetails) {
        if(!Objects.isNull(consolidationDetails))
            return consolidationDetails;
        Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(consolidationId);
        if(optionalConsolidationDetails.isPresent())
            return optionalConsolidationDetails.get();
        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

    public ConsolidationDetails saveConsolidationDGValue(boolean dgFlag, ConsolidationDetails consolidationDetails) {
        if( (!Boolean.TRUE.equals(consolidationDetails.getHazardous()) && dgFlag)
                || (!dgFlag && Boolean.TRUE.equals(consolidationDetails.getHazardous())) ) {
            consolidationDetails.setHazardous(dgFlag);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, dgFlag);
            return consolidationDetails;
        }
        return null;
    }

    public boolean checkIfAllShipmentsAreNonDG(List<Long> shipmentIdList) {
        if (!shipmentIdList.isEmpty()) {
            List<ShipmentDetails> shipmentDetails = shipmentDao.findByShipmentIdInAndContainsHazardous(shipmentIdList, true);
            if(!CollectionUtils.isEmpty(shipmentDetails))
                return false;
        }
        return true;
    }

    public ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Long consolidationId, ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if(makeConsoleDG) {
            consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
            return saveConsolidationDGValue(true, consolidationDetails);
        }
        if(makeConsoleNonDG.get()) {
            List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationId, shipment);
            makeConsoleNonDG.set(checkIfAllShipmentsAreNonDG(shipmentIdList));
            if(makeConsoleNonDG.get()) {
                consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
                return saveConsolidationDGValue(false, consolidationDetails);
            }
        }
        return null;
    }

    private boolean checkForNonDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return !Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private boolean checkForOceanDGShipment(ShipmentDetails shipmentDetails) {
        return TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && Boolean.TRUE.equals(shipmentDetails.getContainsHazardous());
    }

    private boolean checkForNonAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private void processPackUtilisationCalculationInConsole(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentEtV3Request shipmentRequest, ConsolidationDetails linkedConsol) {
        ShipmentRequest shipmentRequest1 = jsonHelper.convertValue(shipmentRequest, ShipmentRequest.class);
        if(linkedConsol != null && shipmentRequest != null) {
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                    .consolidationId(linkedConsol.getId())
                    .saveConsol(true)
                    .shipmentRequest(shipmentRequest1).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
        else if(oldEntity != null && oldEntity.getConsolidationList() != null && !oldEntity.getConsolidationList().isEmpty()) {
            var oldConsolId = oldEntity.getConsolidationList().iterator().next().getId();
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                    .consolidationId(oldConsolId)
                    .saveConsol(true)
                    .shipmentRequest(ShipmentRequest.builder().id(shipment.getId()).build()).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
    }

    @SuppressWarnings("java:S3655")
    private ConsolidationDetails processLinkedConsolidationDetails(ShipmentDetails shipment, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationList, boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        ConsolidationDetails consolidationDetails;
        consolidationDetails = consolidationDetailsDao.findById(consolidationList.iterator().next().getId()).get();
        consolidationDetails.setBol(shipment.getMasterBill());
        if(consolidationDetails.getCarrierDetails() == null)
            consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
        consolidationDetails.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
        consolidationDetails.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
        consolidationDetails.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
        consolidationDetails.setShipmentType(shipment.getDirection());

        if(makeConsoleDG)
            consolidationDetails.setHazardous(true);
        setSendindAndReceivingAgentForNonInterConsole(shipment, consolidationDetails);
        Boolean interBranchConsole = consolidationDetails.getInterBranchConsole();
        List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationList.iterator().next().getId(), shipment);
        if (!shipmentIdList.isEmpty()) {
            processShipmentIdList(shipment, shipmentIdList, interBranchConsole, makeConsoleNonDG, makeConsoleSciT1);
        }
        if(makeConsoleNonDG.get())
            consolidationDetails.setHazardous(false);
        if(makeConsoleSciT1.get() && checkConsoleSciUpdateT1(shipment, oldEntity))
            consolidationDetails.setSci(AwbConstants.T1);
        else if(Objects.equals(consolidationDetails.getSci(), AwbConstants.T1) && !makeConsoleSciT1.get() && oldEntity != null && !Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()))
            consolidationDetails.setSci(null);
        return consolidationDetails;
    }

    private boolean isDiffPresentInOldNewConsole(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        return oldEntity == null || !Objects.equals(shipment.getMasterBill(), oldEntity.getMasterBill()) ||
                !Objects.equals(shipment.getDirection(), oldEntity.getDirection()) ||
                (shipment.getAdditionalDetails() != null && oldEntity.getAdditionalDetails() != null &&
                        (!Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()) ||
                                !CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), oldEntity.getAdditionalDetails().getExportBroker()) ||
                                !CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), oldEntity.getAdditionalDetails().getImportBroker()))) ||
                (shipment.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                        (!Objects.equals(shipment.getCarrierDetails().getVoyage(), oldEntity.getCarrierDetails().getVoyage()) ||
                                !Objects.equals(shipment.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()) ||
                                !Objects.equals(shipment.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
                                !Objects.equals(shipment.getCarrierDetails().getAircraftType(), oldEntity.getCarrierDetails().getAircraftType())
                        ));
    }

    private void setSendindAndReceivingAgentForNonInterConsole(ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (shipment.getAdditionalDetails() != null) {
                if(!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), consolidationDetails.getSendingAgent())) {
                    consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
                }
                if(!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), consolidationDetails.getReceivingAgent())) {
                    consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
                }
            } else {
                consolidationDetails.setSendingAgent(null);
                consolidationDetails.setReceivingAgent(null);
            }
        }
    }

    private void processShipmentIdList(ShipmentDetails shipment, List<Long> shipmentIdList, Boolean interBranchConsole, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(shipmentIdList.stream().collect(
                Collectors.toSet()));
        var a = shipments.stream()
                .map(i -> {
                    i.setMasterBill(shipment.getMasterBill());
                    i.setDirection(shipment.getDirection());
                    if (shipment.getCarrierDetails() != null) {
                        i.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
                        i.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
                        i.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
                        i.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
                    }
                    if(!Boolean.TRUE.equals(interBranchConsole)) {
                        if (shipment.getAdditionalDetails() != null && isExportOrImportBrokerPresent(shipment)) {
                            addAdditionalDetailsForShipment(shipment, i);
                        } else if(shipment.getAdditionalDetails() == null && i.getAdditionalDetails() != null) {
                            i.getAdditionalDetails().setExportBroker(null);
                            i.getAdditionalDetails().setImportBroker(null);
                        }
                    }
                    if (makeConsoleNonDG.get() && Boolean.TRUE.equals(i.getContainsHazardous()))
                        makeConsoleNonDG.set(false);
                    if(Objects.equals(i.getAdditionalDetails().getSci(), AwbConstants.T1)){
                        makeConsoleSciT1.set(true);
                    }
                    return i;
                }).toList();
        shipmentDao.saveAll(a);
    }

    private List<Long> getShipmentIdsExceptCurrentShipment(Long consolidationId, ShipmentDetails shipment) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        return consoleShipmentMappings.stream().filter(c -> !Objects.equals(c.getShipmentId(), shipment.getId()))
                .map(ConsoleShipmentMapping::getShipmentId).toList();
    }
    private boolean checkConsoleSciUpdateT1(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        if(shipment.getAdditionalDetails() == null) return false;
        if(Strings.isNullOrEmpty(shipment.getAdditionalDetails().getSci())) return false;
        if(!Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1)) return false;
        return oldEntity == null || !Objects.equals(shipment.getAdditionalDetails().getSci(),oldEntity.getAdditionalDetails().getSci());
    }

    private boolean isExportOrImportBrokerPresent(ShipmentDetails shipment) {
        return CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getExportBroker()) || CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getImportBroker());
    }

    private void addAdditionalDetailsForShipment(ShipmentDetails shipment, ShipmentDetails i) {
        if(i.getAdditionalDetails() == null) {
            i.setAdditionalDetails(new AdditionalDetails());
        }
        if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), i.getAdditionalDetails().getExportBroker())) {
            i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
        }
        if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), i.getAdditionalDetails().getImportBroker())) {
            i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
        }
    }
}
