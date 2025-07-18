package com.dpw.runner.shipment.services.migration.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@SuppressWarnings("java:S112")
public class ConsolidationMigrationV3Service implements IConsolidationMigrationV3Service {

    @Autowired
    private IShipmentMigrationV3Service shipmentMigrationV3Service;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private IConsolidationV3Service consolidationV3Service;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private IPackingRepository packingRepository;

    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private NotesUtil notesUtil;

    @Override
    @Transactional
    public ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails) {

        Long consolidationId = consolidationDetails.getId();
        log.info("Starting V2 to V3 migration for Consolidation [id={}]", consolidationId);

        ConsolidationDetails consolFromDb = consolidationDetailsDao.findById(consolidationId)
                .orElseThrow(() -> {
                    log.error("No Consolidation found with ID: {}", consolidationId);
                    return new DataRetrievalFailureException("No Console found with given id: " + consolidationId);
                });

        // Step 2: Add notes to existing consolidation (for traceability & audit)
        notesUtil.addNotesForConsolidation(consolFromDb);
        log.info("Notes added for Consolidation [id={}]", consolidationId);

        // This map is used to track which packing maps to which container during migration
        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        // Step 3: Convert V2 console + its attached shipments into V3 structure
        ConsolidationDetails console = mapConsoleV2ToV3(consolFromDb, packingVsContainerGuid);
        log.info("Mapped V2 Consolidation to V3 [id={}]", consolidationId);

        // Step 4: Save all containers separately first, as they must be saved before referencing in packings
        List<Containers> updatedContainersList = console.getContainersList();
        List<Containers> savedUpdatedContainersList = containerRepository.saveAll(updatedContainersList);
        log.info("Saved {} updated container(s) for Consolidation [id={}]", savedUpdatedContainersList.size(), consolidationId);

        // Step 5: Map from GUID to container so we can set containerId in packings
        Map<UUID, Containers> uuidVsUpdatedContainers = savedUpdatedContainersList.stream()
                .collect(Collectors.toMap(Containers::getGuid, Function.identity()));

        // Step 6: Update all packings inside shipments with newly persisted container IDs
        Set<ShipmentDetails> consolShipmentsList = console.getShipmentsList();

        for (ShipmentDetails consolShipment : consolShipmentsList) {
            List<Packing> packingList = consolShipment.getPackingList();
            for (Packing packing : packingList) {
                UUID containerGuid = packingVsContainerGuid.get(packing.getGuid());
                Containers containers = uuidVsUpdatedContainers.get(containerGuid);
                if (containers != null) {
                    packing.setContainerId(containers.getId());
                } else {
                    log.info("No container mapping found for Packing [guid={}] in Shipment [id={}]", packing.getGuid(), consolShipment.getId());
                }
            }
            packingRepository.saveAll(packingList);
            log.info("Saved {} packing(s) for Shipment [id={}]", packingList.size(), consolShipment.getId());
        }

        // Step 7: Update back-reference from shipment → consolidation
        consolShipmentsList.forEach(shp->{
            shp.setConsolidationList(new HashSet<>());
            shp.getConsolidationList().add(console);
            shp.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);
        });

        shipmentRepository.saveAll(consolShipmentsList);
        log.info("Updated {} shipment(s) to link to migrated Consolidation [id={}]", consolShipmentsList.size(), consolidationId);

        // Step 8: Mark consolidation itself as migrated and save
        console.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);
        consolidationRepository.save(console);

        log.info("Migration complete for Consolidation [id={}]", consolidationId);
        return console;
    }

    /**
     * Transforms a V2-style ConsolidationDetails into a V3-compatible object graph. This involves cloning the consolidation, redistributing containers, linking shipments and
     * packings, and updating necessary cut-off fields and computed fields.
     *
     * @param consolidationDetails   V2 entity from DB
     * @param packingVsContainerGuid map to record packing-to-container association during transformation
     * @return transformed V3-compatible consolidation
     */
    public ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails, Map<UUID, UUID> packingVsContainerGuid) {

        UUID consolGuid = consolidationDetails.getGuid();
        log.info("Mapping V2 to V3 for Consolidation [guid={}]", consolGuid);

        // Step 1: Deep clone the V2 consolidation object using JSON helper
        ConsolidationDetails clonedConsole = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        // Defensive fallback
        if (clonedConsole == null) {
            log.error("Failed to deep clone Consolidation [guid={}]", consolGuid);
            throw new IllegalStateException("Failed to clone Consolidation object");
        }

        // Extract shipments from the cloned clonedConsole object
        List<ShipmentDetails> shipmentDetailsList = clonedConsole.getShipmentsList().stream().toList();
        log.info("Cloned Consolidation has {} shipment(s) [guid={}]", shipmentDetailsList.size(), consolGuid);

        // Step 2: Prepare shipment <→> container mappings
        Map<UUID, List<UUID>> containerGuidToShipments = new HashMap<>(); // containerGuid → list of shipmentGuids
        Map<UUID, ShipmentDetails> guidToShipment = new HashMap<>(); // shipmentGuid → shipment

        for (ShipmentDetails shipment : shipmentDetailsList) {
            UUID shipmentGuid = shipment.getGuid();
            guidToShipment.put(shipmentGuid, shipment);

            for (Containers container : shipment.getContainersList()) {
                UUID containerGuid = container.getGuid();
                containerGuidToShipments.computeIfAbsent(containerGuid, k -> new ArrayList<>()).add(shipmentGuid);
            }

            // Step 3a: Clear shipment's containers list (they will be re-linked after splitting)
            shipment.setContainersList(new HashSet<>());

            // Step 3b: Copy relevant cutoff timestamps from console → shipment
            setCutOffProperties(clonedConsole, shipment);
        }

        log.info("Prepared shipment ↔ container mappings for [{}] container(s)", containerGuidToShipments.size());

        // Step 4: Distribute multi-count containers into individual container instances
        List<Containers> splitContainers = distributeContainers(clonedConsole.getContainersList(), containerGuidToShipments);
        clonedConsole.setContainersList(splitContainers);

        Map<UUID, Containers> guidVsContainer = splitContainers.stream().collect(Collectors.toMap(Containers::getGuid, Function.identity()));
        log.info("Distributed containers. Total after split: {}", splitContainers.size());

        // Step 5: Re-link containers <→ shipments for V3 structure
        containerGuidToShipments.forEach((containerGuid, shipmentGuids) -> {
            for (UUID shipmentGuid : shipmentGuids) {
                ShipmentDetails shipment = guidToShipment.get(shipmentGuid);
                Containers container = guidVsContainer.get(containerGuid);

                if (shipment != null && container != null) {
                    shipment.getContainersList().add(container);

                    // Ensure container's reverse reference is also updated
                    if (ObjectUtils.isEmpty(container.getShipmentsList())) {
                        container.setShipmentsList(new HashSet<>());
                    }

                    // Only add reverse link if container is persisted (defensive check)
                    if (container.getId() != null) {
                        container.getShipmentsList().add(shipment);
                    }
                } else {
                    log.info("Failed to re-link container [guid={}] or shipment [guid={}] - possibly missing after clone", containerGuid, shipmentGuid);
                }
            }
        });

        // Step 6: Transform each Shipment to V3 (populates packingVsContainerGuid)
        if (ObjectUtils.isNotEmpty(shipmentDetailsList)) {
            for (ShipmentDetails shipment : shipmentDetailsList) {
                try {
                    shipmentMigrationV3Service.mapShipmentV2ToV3(shipment, packingVsContainerGuid);
                } catch (Exception e) {
                    log.error("Failed to transform Shipment [guid={}] to V3 format", shipment.getGuid(), e);
                    throw new IllegalArgumentException("Shipment transformation failed", e);
                }
            }
        }

        log.info("All shipments transformed to V3 for Consolidation [guid={}]", consolGuid);

        // Step 7: Attach packings to correct containers using packingVsContainerGuid
        assignPackingsToContainers(shipmentDetailsList, packingVsContainerGuid, guidVsContainer);
        log.info("Packings assigned to containers for Consolidation [guid={}]", consolGuid);

        // Step 8: Update console-level summary like weight/volume/counts/etc.
        try {
            consolidationV3Service.calculateAchievedQuantitiesEntity(clonedConsole);
        } catch (Exception e) {
            log.error("Failed to compute achieved quantities for Consolidation [guid={}]", consolGuid, e);
            throw new IllegalArgumentException("Summary calculation failed", e);
        }

        log.info("Completed V2→V3 mapping for Consolidation [guid={}]", consolGuid);
        return clonedConsole;
    }

    private void setCutOffProperties(ConsolidationDetails console, ShipmentDetails shipmentDetails) {
        shipmentDetails.setTerminalCutoff(console.getTerminalCutoff());
        shipmentDetails.setVerifiedGrossMassCutoff(console.getVerifiedGrossMassCutoff());
        shipmentDetails.setShippingInstructionCutoff(console.getShipInstructionCutoff());
        shipmentDetails.setDgCutoff(console.getHazardousBookingCutoff());
        shipmentDetails.setReeferCutoff(console.getReeferCutoff());
        shipmentDetails.setEarliestEmptyEquipmentPickUp(console.getEarliestEmptyEquPickUp());
        shipmentDetails.setLatestFullEquipmentDeliveredToCarrier(console.getLatestFullEquDeliveredToCarrier());
        shipmentDetails.setEarliestDropOffFullEquipmentToCarrier(console.getEarliestDropOffFullEquToCarrier());
        shipmentDetails.setLatestArrivalTime(console.getLatDate());
    }

    private void assignPackingsToContainers(List<ShipmentDetails> shipmentDetailsList, Map<UUID, UUID> packingVsContainerGuid, Map<UUID, Containers> guidVsContainer) {

        log.info("Starting packing-to-container assignment. Packings to map: {}", packingVsContainerGuid.size());

        // Step 1: Pre-clear container fields to re-aggregate packing data from scratch
        packingVsContainerGuid.forEach((packGuid, containerGuid) -> {
            Containers container = guidVsContainer.get(containerGuid);
            if (container != null) {
                container.setGrossWeight(BigDecimal.ZERO);
                container.setGrossWeightUnit(null);
                container.setGrossVolume(BigDecimal.ZERO);
                container.setGrossVolumeUnit(null);
                container.setPacks(null);
                container.setPacksType(null);
                log.info("Cleared aggregation fields for container [guid={}]", containerGuid);
            } else {
                log.info("Container [guid={}] not found while clearing pre-aggregation fields", containerGuid);
            }
        });

        // Step 2: Process each shipment and assign their packings
        for (ShipmentDetails shipment : shipmentDetailsList) {
            UUID shipmentGuid = shipment.getGuid();
            List<Packing> packingList = shipment.getPackingList();

            if (packingList == null || packingList.isEmpty()) {
                log.debug("No packings found for Shipment [guid={}], skipping...", shipmentGuid);
                continue;
            }

            // Prepare packing lookup map for faster access
            Map<UUID, Packing> packingByGuid = packingList.stream()
                    .collect(Collectors.toMap(Packing::getGuid, Function.identity()));

            for (Map.Entry<UUID, Packing> entry : packingByGuid.entrySet()) {
                UUID packingGuid = entry.getKey();
                Packing packing = entry.getValue();

                UUID containerGuid = packingVsContainerGuid.get(packingGuid);
                Containers container = guidVsContainer.get(containerGuid);

                if (container == null) {
                    log.warn("No container found for Packing [guid={}]. Skipping assignment.", packingGuid);
                    continue;
                }

                try {
                    // Add packing data to container aggregation (e.g. weight, volume)
                    containerV3Service.addPackageDataToContainer(container, packing);
                    log.debug("Assigned Packing data [guid={}] to Container [guid={}]", packingGuid, containerGuid);
                } catch (RunnerException e) {
                    log.error("Failed to assign Packing [guid={}] to Container [guid={}]", packingGuid, containerGuid, e);
                    throw new IllegalArgumentException("Failed to add packing to container", e);
                }
            }
        }

        log.info("Completed packing data-to-container assignment for {} shipments", shipmentDetailsList.size());
    }

    @Override
    public ConsolidationDetails migrateConsolidationV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException {
        log.info("Starting V3 to V2 migration for Consolidation [id={}]", consolidationDetails.getId());
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }

        // Convert V3 Console and Attached shipment to V2
        ConsolidationDetails console = mapConsoleV3ToV2(consolidationDetails1.get());
        log.info("Mapped V3 Consolidation to V2 [id={}]", console.getId());
        setMigratedV3Flag(console);

        // ContainerSave
        containerRepository.saveAll(console.getContainersList());
        log.info("Saved updated containers for Consolidation [id={}]", console.getId());
        // PackingSave
        packingRepository.saveAll(console.getPackingList());
        log.info("Saved packings for Consolidation [id={}]", console.getId());
        // ShipmentSave
        shipmentRepository.saveAll(console.getShipmentsList());
        log.info("Updated shipment(s) linked to migrated Consolidation [id={}]", console.getId());
        // ConsoleSave
        consolidationRepository.save(console);
        log.info("Migration V3 to V2 complete for Consolidation [id={}]", console.getId());

        return console;
    }

    private void setMigratedV3Flag(ConsolidationDetails consolidationDetails) {
        consolidationDetails.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V3);

        if(consolidationDetails.getShipmentsList() != null) {
            for (ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                shipmentDetails.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V3);
            }
        }
    }

    public ConsolidationDetails mapConsoleV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        Map<String, EntityTransferContainerType> containerTypeMap = shipmentMigrationV3Service.fetchContainerTypeDetails(console.getContainersList());


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        if(console.getShipmentsList() != null) {
            shipmentDetailsList = console.getShipmentsList().stream().toList();
        }
        if(!CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> {
                try {
                    shipmentMigrationV3Service.mapShipmentV3ToV2(ship, containerTypeMap);
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            });
        }

        // Console utilisation update
        Set<Containers> consoleContainers = setContainerUtilisationForConsolidation(console, shipmentDetailsList, containerTypeMap);

        //consol Packs Utilisation
        if(Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(console.getTransportMode())){
            setPacksUtilisationForConsolidation(console);
        }

        Set<Containers> shipmentContainers = new HashSet<>();
        for (ShipmentDetails shipment : console.getShipmentsList()) {
            shipment.setConsolidationList(new HashSet<>());
            shipment.getConsolidationList().add(console);

            for (Containers container : shipment.getContainersList()) {
                if (container.getShipmentsList() == null) {
                    container.setShipmentsList(new HashSet<>());
                }
                container.getShipmentsList().add(shipment);
                shipmentContainers.add(container);
            }
        }
        Set<Containers> finalContainers = new HashSet<>();
        if (!CommonUtils.setIsNullOrEmpty(consoleContainers)) {
            finalContainers.addAll(consoleContainers);
        }

        if (!CommonUtils.setIsNullOrEmpty(shipmentContainers)) {
            finalContainers.addAll(shipmentContainers);
        }
        console.setContainersList(new ArrayList<>(finalContainers));

        return console;
    }

    private void setPacksUtilisationForConsolidation(ConsolidationDetails consol) throws RunnerException {
        CalculatePackUtilizationRequest calculatePackUtilizationRequest = CalculatePackUtilizationRequest.builder()
                .consolidationId(consol.getId())
                .build();
        PackSummaryResponse packSummaryResponse = packingService.calculatePacksUtilisationForConsolidation(calculatePackUtilizationRequest);
        if(packSummaryResponse.getConsolidationAchievedQuantities() != null && coLoadingConsolChecks(consol)){
            consol.setAchievedQuantities(jsonHelper.convertCreateValue(packSummaryResponse.getConsolidationAchievedQuantities(), AchievedQuantities.class));
            commonUtils.calculateConsolUtilization(consol);
        }
    }

    /**
     * Splits containers with count > 1 into individual 1-count containers.
     * Distributes weight and volume proportionally and rewires shipment mappings accordingly.
     *
     * @param inputContainers             List of containers from V2 (could have count > 1)
     * @param containerGuidToShipments   Mapping of original container GUID to shipment GUIDs (used to reattach new containers)
     * @return A list of containers where each has containerCount == 1
     */
    public List<Containers> distributeContainers(List<Containers> inputContainers, Map<UUID, List<UUID>> containerGuidToShipments) {
        List<Containers> resultContainers = new ArrayList<>();

        for (Containers container : inputContainers) {
            UUID originalGuid = container.getGuid();
            Long count = container.getContainerCount();

            // If count is null or <= 1, keep container as-is (no splitting needed)
            if (count == null || count <= 1) {
                resultContainers.add(container);
                continue;
            }

            log.info("Splitting container [guid={}] with count={}", originalGuid, count);

            List<Containers> tempContainers = new ArrayList<>();

            // Step 1: Split container by duplicating with distributed weight/volume
            distributeMultiCountContainer(container, count, tempContainers);

            // Step 2: For each newly generated container, propagate shipment links from original
            for (Containers tempContainer : tempContainers) {
                // Only copy mapping if it's a new, unsaved container and not already mapped
                if (tempContainer.getId() == null && !containerGuidToShipments.containsKey(tempContainer.getGuid())) {
                    List<UUID> shipmentUuids = containerGuidToShipments.get(originalGuid);
                    if (shipmentUuids != null) {
                        containerGuidToShipments.put(tempContainer.getGuid(), shipmentUuids);
                        log.info("Mapped split container [guid={}] to shipments {}", tempContainer.getGuid(), shipmentUuids);
                    }
                }
            }

            resultContainers.addAll(tempContainers);
        }

        log.info("Finished container splitting. Input: {}, Output: {}", inputContainers.size(), resultContainers.size());
        return resultContainers;
    }

    private void distributeMultiCountContainer(Containers original, Long count, List<Containers> resultContainers) {
        BigDecimal totalWeight = safeBigDecimal(original.getGrossWeight());
        BigDecimal totalVolume = safeBigDecimal(original.getGrossVolume());

        // Divide weight and volume equally, remainder added to last container
        BigDecimal[] weightParts = totalWeight.divideAndRemainder(BigDecimal.valueOf(count));
        BigDecimal[] volumeParts = totalVolume.divideAndRemainder(BigDecimal.valueOf(count));

        BigDecimal baseWeight = weightParts[0];
        BigDecimal weightRemainder = weightParts[1];
        BigDecimal baseVolume = volumeParts[0];
        BigDecimal volumeRemainder = volumeParts[1];

        // Step 1: Convert original container into a 1-count container with base values
        original.setContainerCount(1L);
        original.setGrossWeight(baseWeight);
        original.setGrossVolume(baseVolume);
        resultContainers.add(original);

        // Step 2: Generate new containers for remaining (count - 1)
        for (int i = 1; i < count; i++) {
            boolean isLast = (i == count - 1);

            resultContainers.add(
                    createDistributedCopy(
                            original,
                            baseWeight,
                            baseVolume,
                            isLast ? weightRemainder : BigDecimal.ZERO,
                            isLast ? volumeRemainder : BigDecimal.ZERO
                    )
            );
        }

        log.info("Split container [guid={}] into {} parts", original.getGuid(), count);
    }

    private Containers createDistributedCopy(Containers sourceContainer, BigDecimal baseWeight, BigDecimal baseVolume,
            BigDecimal weightRemainder, BigDecimal volumeRemainder) {
        Containers newContainer = jsonHelper.convertValue(sourceContainer, Containers.class);
        newContainer.setId(null); // Ensure new identity
        newContainer.setGuid(UUID.randomUUID());
        newContainer.setContainerCount(1L);
        newContainer.setGrossWeight(baseWeight.add(weightRemainder));
        newContainer.setGrossVolume(baseVolume.add(volumeRemainder));

        newContainer.setCreatedBy(sourceContainer.getCreatedBy());
        newContainer.setUpdatedBy(sourceContainer.getUpdatedBy());

        log.info("Created split container [guid={}] with weight={} + {} and volume={} + {}",
                newContainer.getGuid(), baseWeight, weightRemainder, baseVolume, volumeRemainder);

        return newContainer;
    }

    private BigDecimal safeBigDecimal(BigDecimal value) {
        return value != null ? value : BigDecimal.ZERO;
    }

    private Set<Containers> setContainerUtilisationForConsolidation(ConsolidationDetails console, List<ShipmentDetails> shipmentDetailsList,
                                                         Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        //Identify container associated only with consolidation and and call setContainerUtilisationForShipment
        Set<Containers> consolContainers = getOnlyConsolidationContainers(console, shipmentDetailsList);
        boolean isFCL = Constants.CARGO_TYPE_FCL.equalsIgnoreCase(console.getShipmentType());
        shipmentMigrationV3Service.setContainerUtilisation(consolContainers, containerTypeMap, isFCL, false);
        return consolContainers;
    }

    private Set<Containers> getOnlyConsolidationContainers(ConsolidationDetails consolidationDetails, List<ShipmentDetails> shipmentDetailsList) {
        if (consolidationDetails == null || consolidationDetails.getContainersList() == null) {
            return Collections.emptySet();
        }

        Set<Long> shipmentContainerIds = shipmentDetailsList.stream()
                .filter(Objects::nonNull)
                .flatMap(shipment -> Optional.ofNullable(shipment.getContainersList())
                        .orElse(Collections.emptySet()).stream())
                .map(Containers::getId)
                .collect(Collectors.toSet());

        return consolidationDetails.getContainersList().stream()
                .filter(container -> !shipmentContainerIds.contains(container.getId()))
                .collect(Collectors.toSet());
    }

    private boolean coLoadingConsolChecks(ConsolidationDetails consol) {
        boolean flag = true;
        if(!consol.getTransportMode().equalsIgnoreCase(TRANSPORT_MODE_AIR))
            flag = false;
        if(!Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()))
            flag = false;
        if(!(consol.getAllocations() != null && consol.getAllocations().getWeight() != null))
            flag = false;
        return flag;
    }


}
