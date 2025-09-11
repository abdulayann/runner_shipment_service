package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.google.common.base.Strings;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
@Generated
@SuppressWarnings("all")
public class ShipmentMigrationV3Service implements IShipmentMigrationV3Service {
    @Autowired
    IV1Service v1Service;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    IContainerService containerService;
    @Autowired
    IShipmentDao shipmentDao;
    @Autowired
    IShipmentRepository shipmentRepository;
    @Autowired
    IContainerRepository containerRepository;
    @Autowired
    IReferenceNumbersRepository referenceNumbersRepository;
    @Autowired
    private HelperExecutor trxExecutor;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IPackingV3Service packingV3Service;
    @Autowired
    private IPackingRepository packingRepository;
    @Autowired
    private NotesUtil notesUtil;
    @Autowired
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;
    @Autowired
    private MigrationUtil migrationUtil;

    private static final List<ShipmentStatus> deprecatedShipmentStatusesForV3 = List.of(ShipmentStatus.Booked, ShipmentStatus.Completed, ShipmentStatus.Confirmed, ShipmentStatus.InTransit, ShipmentStatus.Arrived);
    public static final Map<String, String> airMap = Map.ofEntries(Map.entry("C2P", "C2P"), Map.entry("F2F", "F2F"),
            Map.entry("F2P", "F2A"), Map.entry("P2P", "A2A"), Map.entry("A2A", "A2A"), Map.entry("F2A", "F2A"),
            Map.entry("A2F", "A2F"), Map.entry("C2C", "C2C"), Map.entry("C2F", "C2F"), Map.entry("F2C", "F2C"),
            Map.entry("P2C", "P2C"), Map.entry("P2F", "A2F"));

    public static final Map<String, String> seaMap = Map.ofEntries(Map.entry("C2P", "C2P"), Map.entry("F2F", "F2F"),
            Map.entry("F2P", "F2P"), Map.entry("P2P", "P2P"), Map.entry("A2A", "P2P"), Map.entry("F2A", "F2P"),
            Map.entry("A2F", "P2F"), Map.entry("C2C", "C2C"), Map.entry("C2F", "C2F"), Map.entry("F2C", "F2C"),
            Map.entry("P2C", "P2C"), Map.entry("P2F", "P2F"));

    public static final Map<String, String> railMap = Map.ofEntries(Map.entry("C2P", "C2P"), Map.entry("F2F", "F2F"),
            Map.entry("F2P", "F2P"), Map.entry("P2P", "P2P"), Map.entry("A2A", "P2P"), Map.entry("F2A", "F2P"),
            Map.entry("A2F", "P2F"), Map.entry("C2C", "C2C"), Map.entry("C2F", "C2F"), Map.entry("F2C", "F2C"),
            Map.entry("P2C", "P2C"), Map.entry("P2F", "P2F"));

    public static final Map<String, String> roadMap = Map.ofEntries(Map.entry("C2P", "C2P"), Map.entry("F2F", "F2F"),
            Map.entry("F2P", "F2P"), Map.entry("P2P", "P2P"), Map.entry("A2A", "P2P"), Map.entry("F2A", "F2P"),
            Map.entry("A2F", "P2F"), Map.entry("C2C", "C2C"), Map.entry("C2F", "C2F"), Map.entry("F2C", "F2C"),
            Map.entry("P2C", "P2C"), Map.entry("P2F", "P2F"));


    @Override
    public ShipmentDetails migrateShipmentV2ToV3(Long shipId) throws RunnerException {
        log.info("Starting V2 to V3 migration for Shipment [id={}]", shipId);
        // Handle migration of all the shipments where there is no console attached.
        Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findById(shipId);

        if (shipmentDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Shipment found with given id: " + shipId);
        }
        ShipmentDetails shipment = jsonHelper.convertValue(shipmentDetails1.get(), ShipmentDetails.class);
        notesUtil.addNotesForShipment(shipment);
        log.info("Notes added for Shipment [id={}]", shipment.getId());
        mapShipmentV2ToV3(shipment, new HashMap<>(), true);
        log.info("Mapped V2 Shipment to V3 [id={}]", shipment.getId());

        // Save packing details
        if (!CommonUtils.listIsNullOrEmpty(shipment.getPackingList())) {
            packingRepository.saveAll(shipment.getPackingList());
            log.info("Saved updated packings for Shipment [id={}]", shipment.getId());
        }
        //Save Reference details
        if (!CommonUtils.listIsNullOrEmpty(shipment.getReferenceNumbersList())) {
            referenceNumbersRepository.saveAll(shipment.getReferenceNumbersList());
            log.info("Saved updated references list for Shipment [id={}]", shipment.getId());
        }

        // save shipment
        shipment.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);
        shipment.setTriggerMigrationWarning(true);
        shipmentRepository.save(shipment);
        log.info("Migration V2 to V3 complete for Shipment [id={}]", shipment.getId());
        return shipment;
    }

    @Override
    public ShipmentDetails mapShipmentV2ToV3(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid, Boolean canUpdateTransportInstructions) throws RunnerException {

        // Update Packs based on Auto Update Weight Volume flag
        transformContainerAndPacks(shipmentDetails, packingVsContainerGuid);

        if(Objects.equals(canUpdateTransportInstructions, Boolean.TRUE))
            updateTransportInstruction(shipmentDetails);

        // Migrated shipment fields
        updateShipmentFields(shipmentDetails);

        return shipmentDetails;
    }

    private void updateShipmentFields(ShipmentDetails shipmentDetails) {
        shipmentDetails.setTriggerMigrationWarning(true);
        shipmentDetails.setIsLocked(false);
        migrateServiceTypes(shipmentDetails);

        setCountryFilterInParties(shipmentDetails);

        if (Objects.nonNull(shipmentDetails.getAdditionalDetails())) {
            shipmentDetails.setBrokerageAtDestinationDate(shipmentDetails.getAdditionalDetails().getCustomReleaseDate());
            //Existing values from Additional Details → HBL Details → Agent Reference field should be migrated to the References section with the type AGR
            if (StringUtils.isNotBlank(shipmentDetails.getAdditionalDetails().getAgentReference())) {
                saveReferenceData(shipmentDetails, shipmentDetails.getAdditionalDetails().getAgentReference(), "AGR");
            }
        }

        //Existing values from Pickup → Other Info → UCR Reference field should be migrated to the References section with the type UCR
        if (Objects.nonNull(shipmentDetails.getPickupDetails()) &&
                StringUtils.isNotBlank(shipmentDetails.getPickupDetails().getUcrReference())) {
            saveReferenceData(shipmentDetails, shipmentDetails.getPickupDetails().getUcrReference(), "UCR");
        }

        // migrated deprecated shipment status
        if(shipmentDetails.getStatus() != null && deprecatedShipmentStatusesForV3.contains(ShipmentStatus.fromValue(shipmentDetails.getStatus()))) {
            shipmentDetails.setStatus(ShipmentStatus.Created.getValue());
        }
    }

    private void setCountryFilterInParties(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getClient() != null)
            shipmentDetails.getClient().setCountryCode(shipmentDetails.getClientCountry());
        if(shipmentDetails.getConsigner() != null)
            shipmentDetails.getConsigner().setCountryCode(shipmentDetails.getConsignorCountry());
        if(shipmentDetails.getConsignee() != null)
            shipmentDetails.getConsignee().setCountryCode(shipmentDetails.getConsigneeCountry());
        if(shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getNotifyParty() != null)
            shipmentDetails.getAdditionalDetails().getNotifyParty().setCountryCode(shipmentDetails.getNotifyPartyCountry());
        if(shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getImportBroker() != null)
            shipmentDetails.getAdditionalDetails().getImportBroker().setCountryCode(shipmentDetails.getAdditionalDetails().getImportBrokerCountry());
        if(shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getExportBroker() != null)
            shipmentDetails.getAdditionalDetails().getExportBroker().setCountryCode(shipmentDetails.getAdditionalDetails().getExportBrokerCountry());
        if(shipmentDetails.getShipmentAddresses()!=null && !shipmentDetails.getShipmentAddresses().isEmpty()){
            for(Parties shipmentAddress: shipmentDetails.getShipmentAddresses()){
                if(shipmentAddress.getOrgData()!=null && shipmentAddress.getOrgData().get("Country")!=null)
                    shipmentAddress.setCountryCode((String) shipmentAddress.getOrgData().get("Country"));
            }
        }
    }

    private void saveReferenceData(ShipmentDetails shipmentDetails, String referenceNumber, String referenceType) {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setShipmentId(shipmentDetails.getId());
        referenceNumbers.setReferenceNumber(referenceNumber);
        referenceNumbers.setType(referenceType);
        shipmentDetails.getReferenceNumbersList().add(referenceNumbers);
    }

    private void migrateServiceTypes(ShipmentDetails shipmentDetails){
        if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()) && Objects.nonNull(shipmentDetails.getServiceType())) {
            shipmentDetails.setServiceType(airMap.containsKey(shipmentDetails.getServiceType())? airMap.get(shipmentDetails.getServiceType()) : shipmentDetails.getServiceType());
        }
        if (Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && Objects.nonNull(shipmentDetails.getServiceType())) {
            shipmentDetails.setServiceType(seaMap.containsKey(shipmentDetails.getServiceType())? seaMap.get(shipmentDetails.getServiceType()) : shipmentDetails.getServiceType());
        }
        if (Constants.TRANSPORT_MODE_RAI.equals(shipmentDetails.getTransportMode()) && Objects.nonNull(shipmentDetails.getServiceType())) {
            shipmentDetails.setServiceType(railMap.containsKey(shipmentDetails.getServiceType())? railMap.get(shipmentDetails.getServiceType()) : shipmentDetails.getServiceType());
        }
        if (Constants.TRANSPORT_MODE_ROA.equals(shipmentDetails.getTransportMode()) && Objects.nonNull(shipmentDetails.getServiceType())) {
            shipmentDetails.setServiceType(roadMap.containsKey(shipmentDetails.getServiceType())? roadMap.get(shipmentDetails.getServiceType()) : shipmentDetails.getServiceType());
        }

    }

    private void updateTransportInstruction(ShipmentDetails shipmentDetails) {
        List<PickupDeliveryDetails> pickupDeliveryDetailsInstructions = shipmentDetails.getPickupDeliveryDetailsInstructions();
        int count = 1;
        if (!CollectionUtils.isEmpty(pickupDeliveryDetailsInstructions)) {
            for (PickupDeliveryDetails pickupDeliveryDetails : pickupDeliveryDetailsInstructions) {
                pickupDeliveryDetails.setTiReferenceNumber(shipmentDetails.getShipmentId() + "-" + String.format("%03d", count++));
                TiLegs tiLegs = getTiLegs(pickupDeliveryDetails);
                List<TiLegs> tiLegsList = new ArrayList<>();
                tiLegsList.add(tiLegs);
                pickupDeliveryDetails.setTiLegsList(tiLegsList);
                pickupDeliveryDetailsRepository.save(pickupDeliveryDetails);
            }
        }
    }

    private static TiLegs getTiLegs(PickupDeliveryDetails pickupDeliveryDetails) {
        TiLegs tiLegs = new TiLegs();
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setPickupDeliveryDetailsId(pickupDeliveryDetails.getId());
        tiLegs.setSequence(1L);
        tiLegs.setEstimatedPickup(pickupDeliveryDetails.getEstimatedPickup());
        tiLegs.setEstimatedDelivery(pickupDeliveryDetails.getEstimatedDelivery());
        tiLegs.setActualPickup(pickupDeliveryDetails.getActualPickup());
        tiLegs.setActualDelivery(pickupDeliveryDetails.getActualDelivery());
        tiLegs.setDropMode(pickupDeliveryDetails.getDropMode());
        tiLegs.setRemarks(pickupDeliveryDetails.getRemarks());
        tiLegs.setRequiredBy(pickupDeliveryDetails.getRequiredBy());
        tiLegs.setOrigin(pickupDeliveryDetails.getSourceDetail());
        tiLegs.setDestination(pickupDeliveryDetails.getDestinationDetail());
        return tiLegs;
    }

    private void transformContainerAndPacks(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        if (Constants.SHIPMENT_TYPE_LCL.equals(shipmentDetails.getShipmentType()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            createPacksForUnassignedContainers(shipmentDetails, packingVsContainerGuid);
        }
        if(!CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList())) {
            shipmentDetails.getPackingList().forEach(pack -> {
                pack.setVolume(pack.getVolume() == null? BigDecimal.ZERO: pack.getVolume());
                pack.setVolumeUnit(Strings.isNullOrEmpty(pack.getVolumeUnit())? Constants.VOLUME_UNIT_M3: pack.getVolumeUnit());
            });
        }
    }

    private void createPackWithContainerInfo(Packing packing, Containers container) {
        packing.setPacks("0");
        packing.setPacksType(PackingConstants.PKG);
        packing.setVolume(BigDecimal.ZERO);
        packing.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        packing.setCommodity("MISC");
    }

    private void createPacksForUnassignedContainers(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        List<Containers> unassignedContainers = new ArrayList<>(shipmentDetails.getContainersList().stream()
                .filter(containers -> ObjectUtils.isEmpty(containers.getId())).toList());

        Map<Long, Containers> containerMap = shipmentDetails.getContainersList().stream()
                .filter(containers -> ObjectUtils.isNotEmpty(containers.getId()))
                .collect(Collectors.toMap(BaseEntity::getId, Function.identity()));

        shipmentDetails.getPackingList().forEach(packing -> containerMap.remove(packing.getContainerId()));

        unassignedContainers.addAll(containerMap.values());

        if (!unassignedContainers.isEmpty()) {
            for (var cont : unassignedContainers) {
                Packing packing = new Packing();
                packing.setGuid(UUID.randomUUID());
                packing.setShipmentId(shipmentDetails.getId());
                packing.setCreatedBy(shipmentDetails.getCreatedBy());
                packing.setUpdatedBy(shipmentDetails.getUpdatedBy());
                packingVsContainerGuid.putIfAbsent(packing.getGuid(), cont.getGuid());
                // populate Container info to pack
                createPackWithContainerInfo(packing, cont);
                shipmentDetails.getPackingList().add(packing);
            }
        }
    }

    @Override
    public ShipmentDetails migrateShipmentV3ToV2(Long shipId) throws RunnerException {
        log.info("Starting V3 to V2 migration for Shipment [id={}]", shipId);
        Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findById(shipId);
        if (shipmentDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Shipment found with given id: " + shipId);
        }
        ShipmentDetails shipment = jsonHelper.convertValue(shipmentDetails1.get(), ShipmentDetails.class);
        Map<String, EntityTransferContainerType> containerTypeMap = fetchContainerTypeDetails(shipment.getContainersList());
        mapShipmentV3ToV2(shipment, containerTypeMap);
        log.info("Mapped V3 Shipment to V2 [id={}]", shipment.getId());

        if (!CommonUtils.setIsNullOrEmpty(shipment.getContainersList())) {
            containerRepository.saveAll(shipment.getContainersList().stream().toList());
            log.info("Saved updated containers for Shipment [id={}]", shipment.getId());
        }
        shipment.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V3);
        // save shipment
        shipmentRepository.save(shipment);
        log.info("Migration V3 to V2 complete for Shipment [id={}]", shipment.getId());
        return shipment;
    }

    @Override
    public ShipmentDetails mapShipmentV3ToV2(ShipmentDetails shipmentDetails, Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        // Business Logic for transformation
        // need to add shipment details transformation logic
        shipmentDetails.setAutoUpdateWtVol(true);
        shipmentDetails.setContainerAutoWeightVolumeUpdate(false);

        // update container utilisation
        setContainerUtilisationForShipment(shipmentDetails, containerTypeMap);
        shipmentDetails.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V3);
        return shipmentDetails;
    }

    public void setContainerUtilisationForShipment(ShipmentDetails shipmentDetails, Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        if (CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            return;
        }
        boolean isFCL = Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType());
        setContainerUtilisation(shipmentDetails.getContainersList(), containerTypeMap, isFCL, true);
    }

    public void setContainerUtilisation(Set<Containers> containers, Map<String, EntityTransferContainerType> containerTypeMap, boolean isFCL, boolean isAttached) throws RunnerException {
        for (Containers container : containers) {
            container.setIsAttached(isAttached);
            if (containerTypeMap.containsKey(container.getContainerCode())) {
                EntityTransferContainerType containerType = containerTypeMap.get(container.getContainerCode());
                if (containerType.getMaxCargoGrossWeight() != null) {
                    container.setAllocatedWeight(BigDecimal.valueOf(containerType.getMaxCargoGrossWeight()));
                }
                container.setAllocatedWeightUnit(containerType.getMaxCargoGrossWeightUnit());

                if (containerType.getCubicCapacity() != null) {
                    container.setAllocatedVolume(BigDecimal.valueOf(containerType.getCubicCapacity()));
                }
                container.setAllocatedVolumeUnit(containerType.getCubicCapacityUnit());

                if (isFCL) {
                    container.setAchievedWeight(container.getAllocatedWeight());
                    container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                    container.setAchievedVolume(container.getAllocatedVolume());
                    container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                } else {
                    setContainerValuesForLCL(container, containerType);
                }
            }
            containerService.calculateUtilization(container);
        }
    }

    private void setContainerValuesForLCL(Containers containers, EntityTransferContainerType containerType) throws RunnerException {
        if(containers.getShipmentsList()!=null && !containers.getShipmentsList().isEmpty()) {
            if (containers.getGrossWeight() != null && !isStringNullOrEmpty(containers.getGrossWeightUnit())
                    && !isStringNullOrEmpty(containerType.getMaxCargoGrossWeightUnit()) && containerType.getMaxCargoGrossWeight() != null) {
                Double weight = (Double) convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), containerType.getMaxCargoGrossWeightUnit());
                containers.setAchievedWeight(BigDecimal.valueOf(weight));
            }
            if (containers.getGrossVolume() != null && !isStringNullOrEmpty(containers.getGrossVolumeUnit())
                    && !isStringNullOrEmpty(containerType.getCubicCapacityUnit()) && containerType.getCubicCapacity() != null) {
                Double volume = (Double) convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), containerType.getCubicCapacityUnit());
                containers.setAchievedVolume(BigDecimal.valueOf(volume));
            }
        }
    }

    private Map<String, EntityTransferContainerType> fetchContainerTypeDetails(Set<Containers> containers) {
        if (CommonUtils.setIsNullOrEmpty(containers))
            return new HashMap<>();
        return fetchContainerTypeDetails(containers.stream().toList());
    }

    public Map<String, EntityTransferContainerType> fetchContainerTypeDetails(List<Containers> containers) {
        if (CommonUtils.listIsNullOrEmpty(containers))
            return new HashMap<>();
        Set<String> containerTypeCodes = new HashSet<>();
        containers.forEach(container -> containerTypeCodes.add(container.getContainerCode()));
        CommonV1ListRequest listRequest = new CommonV1ListRequest();
        List<Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.CODE),
                "In",
                List.of(containerTypeCodes.stream().toList())
        );
        listRequest.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(listRequest);
        Map<String, EntityTransferContainerType> containerTypeMap = new HashMap<>();
        if (v1DataResponse != null && v1DataResponse.entities != null) {
            List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferContainerType.class);
            if (!containerTypesList.isEmpty()) {
                containerTypesList.forEach(entityTransferContainerType -> containerTypeMap.put(entityTransferContainerType.Code, entityTransferContainerType));
            }
        }
        return containerTypeMap;
    }

    public Map<String, Integer> migrateShipmentsV3ToV2ForTenant(Integer tenantId) {
        Map<String, Integer> stats = new HashMap<>();
        List<Long> shipmentIds = fetchShipmentFromDB(List.of(MigrationStatus.CREATED_IN_V3.name(), MigrationStatus.MIGRATED_FROM_V2.name()), tenantId);

        log.info("[ShipmentMigration] Tenant [{}]: Found [{}] shipment(s) to migrate.", tenantId, shipmentIds.size());
        stats.put("Total Shipment", shipmentIds.size());

        List<Future<Long>> futures = new ArrayList<>();

        shipmentIds.forEach(id -> futures.add(trxExecutor.runInAsync(() -> {
            try {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.getUser().setPermissions(new HashMap<>());

                return trxExecutor.runInTrx(() -> {
                    log.info("[ShipmentMigration] [Tenant: {}, ShipmentId: {}] Starting migration...", tenantId, id);
                    ShipmentDetails migrated = null;
                    try {
                        migrated = migrateShipmentV3ToV2(id);
                    } catch (RunnerException e) {
                        throw new RuntimeException(e);
                    }
                    log.info("[ShipmentMigration] [Tenant: {}, OldId: {}, NewId: {}] Migration successful.", tenantId, id, migrated.getId());
                    return migrated.getId();
                });
            } catch (Exception e) {
                log.error("[ShipmentMigration] [Tenant: {}, ShipmentId: {}] Migration failed: {}", tenantId, id, e.getMessage(), e);
                migrationUtil.saveErrorResponse(id, Constants.SHIPMENT, IntegrationType.V3_TO_V2_DATA_SYNC, Status.FAILED, Arrays.toString(e.getStackTrace()));
                throw new IllegalArgumentException(e);
            } finally {
                v1Service.clearAuthContext();
            }
        })));

        List<Long> migratedIds = MigrationUtil.collectAllProcessedIds(futures);
        stats.put("Total Shipment Migrated", migratedIds.size());

        log.info("[ShipmentMigration] Tenant [{}]: {}/{} shipments migrated successfully.",
                tenantId, migratedIds.size(), shipmentIds.size());

        return stats;
    }

    private List<Long> fetchShipmentFromDB(List<String> migrationStatuses, Integer tenantId) {
        return shipmentDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

}
