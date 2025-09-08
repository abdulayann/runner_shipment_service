package com.dpw.runner.shipment.services.migration.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import javax.transaction.Transactional;

import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@Generated
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

    @Autowired
    private V1ServiceImpl v1Service;

    @Autowired
    private HelperExecutor trxExecutor;

    @Autowired
    private MigrationUtil migrationUtil;

    @Autowired
    private IReferenceNumbersRepository referenceNumbersRepository;


    @Transactional
    @Override
    public ConsolidationDetails migrateConsolidationV2ToV3(Long consolidationId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal) {

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
        ConsolidationDetails console = mapConsoleV2ToV3(consolFromDb, packingVsContainerGuid, true, codeTeuMap, weightDecimal, volumeDecimal);
        log.info("Mapped V2 Consolidation to V3 [id={}]", consolidationId);

        // Step 4: Removed linkage of containers with booking as new Containers are created in booking
        List<Containers> updatedContainersList = console.getContainersList();

        // Step 5: Save all containers separately first, as they must be saved before referencing in packings
        List<Containers> savedUpdatedContainersList = containerRepository.saveAll(updatedContainersList);
        log.info("Saved {} updated container(s) for Consolidation [id={}]", savedUpdatedContainersList.size(), consolidationId);

        // Step 5: Map from GUID to container so we can set containerId in packings
        Map<UUID, Containers> uuidVsUpdatedContainers = savedUpdatedContainersList.stream()
                .collect(Collectors.toMap(Containers::getGuid, Function.identity()));

        // Step 6: Update all packings inside shipments with newly persisted container IDs
        Set<ShipmentDetails> consolShipmentsList = console.getShipmentsList();

        for (ShipmentDetails consolShipment : consolShipmentsList) {
            List<Packing> packingList = consolShipment.getPackingList();
            List<ReferenceNumbers> referenceNumbersList = consolShipment.getReferenceNumbersList();
            for (Packing packing : packingList) {
                UUID containerGuid = packingVsContainerGuid.get(packing.getGuid());
                Containers containers = uuidVsUpdatedContainers.get(containerGuid);
                if (containers != null) {
                    packing.setContainerId(containers.getId());
                } else {
                    log.info("No container mapping found for Packing [guid={}] in Shipment [id={}]", packing.getGuid(), consolShipment.getId());
                }
            }
            referenceNumbersRepository.saveAll(referenceNumbersList);
            packingRepository.saveAll(packingList);
            log.info("Saved {} packing(s) for Shipment [id={}]", packingList.size(), consolShipment.getId());
        }

        // Step 7: Update back-reference from shipment → consolidation
        consolShipmentsList.forEach(shp->{
            shp.setConsolidationList(new HashSet<>());
            shp.getConsolidationList().add(console);
            shp.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);
            shp.setTriggerMigrationWarning(true);
        });

        shipmentRepository.saveAll(consolShipmentsList);
        log.info("Updated {} shipment(s) to link to migrated Consolidation [id={}]", consolShipmentsList.size(), consolidationId);

        // Step 8: Mark consolidation itself as migrated and save
        setMigrationStatusEnum(console, MigrationStatus.MIGRATED_FROM_V2);
        console.setTriggerMigrationWarning(true);
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
    public ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails, Map<UUID, UUID> packingVsContainerGuid, Boolean canUpdateTransportInstructions, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal) {

        if(codeTeuMap.isEmpty()){
            codeTeuMap = migrationUtil.initCodeTeuMap();
        }

        UUID consolGuid = consolidationDetails.getGuid();
        log.info("Mapping V2 to V3 for Consolidation [guid={}]", consolGuid);

        // Step 1: Deep clone the V2 consolidation object using JSON helper
        ConsolidationDetails clonedConsole = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        // Defensive fallback
        if (clonedConsole == null) {
            log.error("Failed to deep clone Consolidation [guid={}]", consolGuid);
            throw new IllegalStateException("Failed to clone Consolidation object");
        }

        setConsolidationFields(clonedConsole);

        // Extract shipments from the cloned clonedConsole object
        List<ShipmentDetails> shipmentDetailsList = clonedConsole.getShipmentsList() == null
                ? new ArrayList<>() : clonedConsole.getShipmentsList().stream().toList();
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
        List<Containers> splitContainers = distributeContainers(clonedConsole.getContainersList(), containerGuidToShipments, codeTeuMap, weightDecimal, volumeDecimal);
        clonedConsole.setContainersList(splitContainers);
        for(Containers updatedContainer: splitContainers) {
            updatedContainer.setBookingId(null);
        }


        Map<UUID, Containers> guidVsContainer = splitContainers.stream().collect(Collectors.toMap(Containers::getGuid, Function.identity()));
        log.info("Distributed containers. Total after split: {}", splitContainers.size());

        // Step 5: Re-link containers <→ shipments for V3 structure
        relinkContainerToShipment(containerGuidToShipments, guidToShipment, guidVsContainer);

        // Step 6: Transform each Shipment to V3 (populates packingVsContainerGuid)
        if (ObjectUtils.isNotEmpty(shipmentDetailsList)) {
            for (ShipmentDetails shipment : shipmentDetailsList) {
                try {
                    shipmentMigrationV3Service.mapShipmentV2ToV3(shipment, packingVsContainerGuid, canUpdateTransportInstructions);
                } catch (Exception e) {
                    log.error("Failed to transform Shipment [guid={}] to V3 format", shipment.getGuid(), e);
                    throw new IllegalArgumentException("Shipment transformation failed", e);
                }
            }
        }
        // Step 7: Console summary update
        try {
            if(Objects.isNull(clonedConsole.getAchievedQuantities()))
                clonedConsole.setAchievedQuantities(new AchievedQuantities());
            consolidationV3Service.calculateAchievedQuantitiesEntity(clonedConsole);
        } catch (Exception e){
            log.error("Failed to calculate AchievedQuantitiesEntity for console [id={}] to V3 format", clonedConsole.getId(), e);
        }

        log.info("All shipments transformed to V3 for Consolidation [guid={}]", consolGuid);

        clonedConsole.setOpenForAttachment(true);
        clonedConsole.setTriggerMigrationWarning(true);
        clonedConsole.setIsLocked(false);
        clonedConsole.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);
        log.info("Completed V2→V3 mapping for Consolidation [guid={}]", consolGuid);
        return clonedConsole;
    }

    private void setConsolidationFields(ConsolidationDetails consolidationDetails) {
        if(consolidationDetails.getSendingAgent() != null)
            consolidationDetails.getSendingAgent().setCountryCode(consolidationDetails.getSendingAgentCountry());
        if(consolidationDetails.getReceivingAgent() != null)
            consolidationDetails.getReceivingAgent().setCountryCode(consolidationDetails.getReceivingAgentCountry());
        if(consolidationDetails.getConsolidationAddresses()!=null && !consolidationDetails.getConsolidationAddresses().isEmpty()){
            for(Parties consolidationAddress: consolidationDetails.getConsolidationAddresses()){
                if(consolidationAddress.getOrgData()!=null && consolidationAddress.getOrgData().get("Country")!=null)
                    consolidationAddress.setCountryCode((String) consolidationAddress.getOrgData().get("Country"));
            }
        }
    }

    private void relinkContainerToShipment(Map<UUID, List<UUID>> containerGuidToShipments, Map<UUID, ShipmentDetails> guidToShipment, Map<UUID, Containers> guidVsContainer) {
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
    }

    private void setCutOffProperties(ConsolidationDetails console, ShipmentDetails shipmentDetails) {
        shipmentDetails.setConsolRef(console.getConsolidationNumber());
        if(Objects.equals(console.getShipmentType(), Constants.DIRECTION_DOM))
            return;

        if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_SEA)) {
            shipmentDetails.setTerminalCutoff(console.getTerminalCutoff());
            shipmentDetails.setVerifiedGrossMassCutoff(console.getVerifiedGrossMassCutoff());
            shipmentDetails.setShippingInstructionCutoff(console.getShipInstructionCutoff());
            shipmentDetails.setDgCutoff(console.getHazardousBookingCutoff());
            shipmentDetails.setReeferCutoff(console.getReeferCutoff());
            shipmentDetails.setEarliestEmptyEquipmentPickUp(console.getEarliestEmptyEquPickUp());
            shipmentDetails.setLatestFullEquipmentDeliveredToCarrier(console.getLatestFullEquDeliveredToCarrier());
            shipmentDetails.setEarliestDropOffFullEquipmentToCarrier(console.getEarliestDropOffFullEquToCarrier());
        }
        if(Objects.equals(console.getTransportMode(), TRANSPORT_MODE_AIR))
            shipmentDetails.setLatestArrivalTime(console.getLatDate());
    }

    @Override
    public ConsolidationDetails migrateConsolidationV3ToV2(Long consoleId) throws RunnerException {
        log.info("Starting V3 to V2 migration for Consolidation [id={}]", consoleId);
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consoleId);
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consoleId);
        }

        // Convert V3 Console and Attached shipment to V2
        ConsolidationDetails console = mapConsoleV3ToV2(consolidationDetails1.get());
        log.info("Mapped V3 Consolidation to V2 [id={}]", console.getId());
        setMigrationStatusEnum(console, MigrationStatus.MIGRATED_FROM_V3);

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

    private void setMigrationStatusEnum(ConsolidationDetails consolidationDetails, MigrationStatus migrationStatus) {
        consolidationDetails.setMigrationStatus(migrationStatus);

        if(consolidationDetails.getShipmentsList() != null) {
            for (ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                shipmentDetails.setMigrationStatus(migrationStatus);
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
        setMigrationStatusEnum(console, MigrationStatus.MIGRATED_FROM_V3);

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
    public List<Containers> distributeContainers(List<Containers> inputContainers, Map<UUID, List<UUID>> containerGuidToShipments, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal) {
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
            distributeMultiCountContainer(container, count, tempContainers, weightDecimal, volumeDecimal);

            // Step 2: For each newly generated container, propagate shipment links from original
            for (Containers tempContainer : tempContainers) {
                setTeuInContainers(codeTeuMap, tempContainer);
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

    private void setTeuInContainers(Map<String, BigDecimal> codeTeuMap, Containers tempContainer) {
        if (!codeTeuMap.isEmpty() && codeTeuMap.containsKey(tempContainer.getContainerCode())) {
            tempContainer.setTeu(codeTeuMap.get(tempContainer.getContainerCode()));
        }
    }

    private void distributeMultiCountContainer(Containers original, Long count, List<Containers> resultContainers, Integer weightDecimal, Integer volumeDecimal) { //NOSONAR
        BigDecimal totalWeight = safeBigDecimal(original.getGrossWeight());
        BigDecimal totalVolume = safeBigDecimal(original.getGrossVolume());
        BigDecimal tareWeight = safeBigDecimal(original.getTareWeight());
        BigDecimal netWeight = safeBigDecimal(original.getNetWeight());
        BigDecimal totalPacks = StringUtility.isNotEmpty(original.getPacks()) ? BigDecimal.valueOf(Long.parseLong(original.getPacks())): BigDecimal.ZERO;

        // Divide weight and volume equally, remainder added to last container
        BigDecimal[] weightParts = totalWeight.divideAndRemainder(BigDecimal.valueOf(count));
        BigDecimal[] volumeParts = totalVolume.divideAndRemainder(BigDecimal.valueOf(count));
        BigDecimal[] packsParts = totalPacks.divideAndRemainder(BigDecimal.valueOf(count));
        BigDecimal[] tareWeightParts = tareWeight.divideAndRemainder(BigDecimal.valueOf(count));
        BigDecimal[] netWeightParts = netWeight.divideAndRemainder(BigDecimal.valueOf(count));

        BigDecimal baseWeight = weightParts[0];
        BigDecimal weightRemainder = weightParts[1];
        weightRemainder = weightRemainder.setScale(weightDecimal, RoundingMode.HALF_UP);
        BigDecimal baseVolume = volumeParts[0];
        BigDecimal volumeRemainder = volumeParts[1];
        volumeRemainder = volumeRemainder.setScale(volumeDecimal, RoundingMode.HALF_UP);
        BigDecimal basePacks = packsParts[0];
        BigDecimal packsRemainder = packsParts[1];
        BigDecimal baseTareWeight = tareWeightParts[0];
        BigDecimal tareWeightRemainder = tareWeightParts[1];
        tareWeightRemainder = tareWeightRemainder.setScale(weightDecimal, RoundingMode.HALF_UP);
        BigDecimal baseNetWeight = netWeightParts[0];
        BigDecimal netWeightRemainder = netWeightParts[1];
        netWeightRemainder = netWeightRemainder.setScale(weightDecimal, RoundingMode.HALF_UP);

        // Step 1: Convert original container into a 1-count container with base values
        original.setContainerCount(1L);
        original.setGrossWeight(baseWeight);
        if(weightRemainder.intValue() >= 1) {
            BigDecimal fractionalPart = weightRemainder.remainder(BigDecimal.ONE);
            original.setGrossWeight(baseWeight.add(BigDecimal.ONE).add(fractionalPart));
            weightRemainder = weightRemainder.subtract(BigDecimal.ONE).subtract(fractionalPart);
        } else{
            original.setGrossWeight(baseWeight.add(weightRemainder));
            weightRemainder = BigDecimal.ZERO;
        }

        original.setGrossVolume(baseVolume);
        if(volumeRemainder.intValue() >= 1) {
            BigDecimal fractionalPart = volumeRemainder.remainder(BigDecimal.ONE);
            original.setGrossVolume(baseVolume.add(BigDecimal.ONE).add(fractionalPart));
            volumeRemainder = volumeRemainder.subtract(BigDecimal.ONE).subtract(fractionalPart);
        }else{
            original.setGrossVolume(baseVolume.add(volumeRemainder));
            volumeRemainder = BigDecimal.ZERO;
        }
        int packsCount = basePacks.intValue();
        if(packsRemainder.intValue() >= 1) {
            packsCount += 1;
            packsRemainder = packsRemainder.subtract(BigDecimal.valueOf(1));
        }
        original.setPacks(String.valueOf(packsCount));

        original.setTareWeight(baseTareWeight);
        if(tareWeightRemainder.intValue() >= 1) {
            BigDecimal fractionalPart = tareWeightRemainder.remainder(BigDecimal.ONE);
            original.setTareWeight(baseTareWeight.add(BigDecimal.ONE).add(fractionalPart));
            tareWeightRemainder = tareWeightRemainder.subtract(BigDecimal.ONE).subtract(fractionalPart);
        }else{
            original.setTareWeight(baseTareWeight.add(tareWeightRemainder));
            tareWeightRemainder = BigDecimal.ZERO;
        }

        original.setNetWeight(baseNetWeight);
        if(netWeightRemainder.intValue() >= 1) {
            BigDecimal fractionalPart = netWeightRemainder.remainder(BigDecimal.ONE);
            original.setNetWeight(baseNetWeight.add(BigDecimal.ONE).add(fractionalPart));
            netWeightRemainder = netWeightRemainder.subtract(BigDecimal.ONE).subtract(fractionalPart);
        }else{
            original.setNetWeight(baseNetWeight.add(netWeightRemainder));
            netWeightRemainder = BigDecimal.ZERO;
        }

        resultContainers.add(original);

        // Step 2: Generate new containers for remaining (count - 1)
        for (int i = 1; i < count; i++) {

            resultContainers.add(
                    createDistributedCopy(
                            original, baseWeight, baseVolume, weightRemainder, volumeRemainder,
                            basePacks, packsRemainder, baseTareWeight, tareWeightRemainder, baseNetWeight, netWeightRemainder
                    )
            );
            if(weightRemainder.intValue() >= 1) {
                weightRemainder = weightRemainder.subtract(BigDecimal.valueOf(1));
            }
            if(volumeRemainder.intValue() >= 1) {
                volumeRemainder = volumeRemainder.subtract(BigDecimal.valueOf(1));
            }
            if(packsRemainder.intValue() >= 1) {
                packsRemainder = packsRemainder.subtract(BigDecimal.valueOf(1));
            }
            if(tareWeightRemainder.intValue() >= 1) {
                tareWeightRemainder = tareWeightRemainder.subtract(BigDecimal.valueOf(1));
            }
            if(netWeightRemainder.intValue() >= 1) {
                netWeightRemainder = netWeightRemainder.subtract(BigDecimal.valueOf(1));
            }
        }

        log.info("Split container [guid={}] into {} parts", original.getGuid(), count);
    }

    private Containers createDistributedCopy(Containers sourceContainer, BigDecimal baseWeight, BigDecimal baseVolume,
                                             BigDecimal weightRemainder, BigDecimal volumeRemainder, BigDecimal basePacks, BigDecimal packsRemainder,
                                             BigDecimal baseTareWeight, BigDecimal tareWeightRemainder,
                                             BigDecimal baseNetWeight, BigDecimal netWeightRemainder
    ) {
        Containers newContainer = jsonHelper.convertValue(sourceContainer, Containers.class);
        newContainer.setId(null); // Ensure new identity
        newContainer.setGuid(UUID.randomUUID());
        newContainer.setContainerCount(1L);

        newContainer.setGrossWeight(baseWeight);
        if(weightRemainder.intValue() >= 1) {
            newContainer.setGrossWeight(baseWeight.add(BigDecimal.valueOf(1)));
        }
        newContainer.setGrossVolume(baseVolume);
        if(volumeRemainder.intValue() >= 1) {
            newContainer.setGrossVolume(baseVolume.add(BigDecimal.valueOf(1)));
        }
        int packsCount = basePacks.intValue();
        if(packsRemainder.intValue() >= 1) {
            packsCount += 1;
        }
        newContainer.setPacks(String.valueOf(packsCount));

        newContainer.setTareWeight(baseTareWeight);
        if(tareWeightRemainder.intValue() >= 1) {
            newContainer.setTareWeight(baseTareWeight.add(BigDecimal.valueOf(1)));
        }
        newContainer.setNetWeight(baseNetWeight);
        if(netWeightRemainder.intValue() >= 1) {
            newContainer.setNetWeight(baseNetWeight.add(BigDecimal.valueOf(1)));
        }

        newContainer.setCreatedBy(sourceContainer.getCreatedBy());
        newContainer.setUpdatedBy(sourceContainer.getUpdatedBy());

        log.info("Created split container [guid={}] with weight={} + {} and volume={} + {} and packs={} + {}",
                newContainer.getGuid(), baseWeight, weightRemainder, baseVolume, volumeRemainder, basePacks, packsRemainder);

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

    public Map<String, Integer> migrateConsolidationsV3ToV2ForTenant(Integer tenantId) {
        Map<String, Integer> stats = new HashMap<>();
        List<Long> consolidationDetailsIds = fetchConsoleFromDB(List.of(MigrationStatus.CREATED_IN_V3.name(), MigrationStatus.MIGRATED_FROM_V2.name()), tenantId);

        log.info("[ConsolidationMigration] Tenant [{}]: Found [{}] consolidation(s) to migrate.", tenantId, consolidationDetailsIds.size());
        stats.put("Total Consolidation", consolidationDetailsIds.size());

        List<Future<Long>> futures = new ArrayList<>();

        for (Long consoleIds : consolidationDetailsIds) {
            futures.add(trxExecutor.runInAsync(() -> {
                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        log.info("[ConsolidationMigration] [Tenant: {}, ConsoleId: {}] Starting migration...", tenantId, consoleIds);
                        ConsolidationDetails migrated = null;
                        try {
                            migrated = migrateConsolidationV3ToV2(consoleIds);
                        } catch (RunnerException e) {
                            throw new RuntimeException(e);
                        }
                        log.info("[ConsolidationMigration] [Tenant: {}, OldId: {}, NewId: {}] Migration successful.", tenantId, consoleIds, migrated.getId());
                        return migrated.getId();
                    });
                } catch (Exception e) {
                    log.error("[ConsolidationMigration] [Tenant: {}, ConsoleId: {}] Migration failed: {}", tenantId, consoleIds, Arrays.toString(e.getStackTrace()), e);
                    migrationUtil.saveErrorResponse(consoleIds, Constants.CONSOLIDATION, IntegrationType.V3_TO_V2_DATA_SYNC, Status.FAILED, e.getStackTrace().toString());
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            }));
        }

        List<Long> migratedIds = MigrationUtil.collectAllProcessedIds(futures);
        stats.put("Total Consolidation Migrated", migratedIds.size());

        log.info("[ConsolidationMigration] Tenant [{}]: {}/{} consolidations migrated successfully.",
                tenantId, migratedIds.size(), consolidationDetailsIds.size());

        return stats;
    }

    private List<Long> fetchConsoleFromDB(List<String> migrationStatuses, Integer tenantId) {
        return consolidationDetailsDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }


}
