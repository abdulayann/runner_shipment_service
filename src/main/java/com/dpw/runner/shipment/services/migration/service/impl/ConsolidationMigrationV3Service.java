package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
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
public class ConsolidationMigrationV3Service implements IConsolidationMigrationV3Service {

    @Autowired
    private IShipmentMigrationV3Service shipmentMigrationV3Service;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private IConsolidationV3Service consolidationV3Service;

    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private IPackingRepository packingRepository;

    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Override
    @Transactional
    public ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails) {
        ConsolidationDetails consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId())
                .orElseThrow(() -> new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId()));

        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        // Convert V2 Console and Attached shipment to V3
        ConsolidationDetails console = mapConsoleV2ToV3(consolidationDetails1, packingVsContainerGuid);

        // ContainerSave
        List<Containers> updatedContainersList = console.getContainersList();
        List<Containers> savedUpdatedContainersList = containerRepository.saveAll(updatedContainersList);
        Map<UUID, Containers> uuidVsUpdatedContainers = savedUpdatedContainersList.stream().collect(Collectors.toMap(Containers::getGuid, Function.identity()));


        //TODO guid to container -> shipment's container store
        // Attach Container to shipment
        Set<ShipmentDetails> consolShipmentsList = console.getShipmentsList();
//        for (ShipmentDetails shp : consolShipmentsList) {
//            Set<Containers> originalContainers = shp.getContainersList();
//            Set<Containers> updatedContainers = new HashSet<>();
//
//            for (Containers originalContainer : originalContainers) {
//                Containers updatedContainer = uuidVsUpdatedContainers.get(originalContainer.getGuid());
//                updatedContainers.add(Objects.requireNonNullElse(updatedContainer, originalContainer));
//            }
//            shp.setContainersList(updatedContainers);
//        }






        // PackingSave
        // TODO: replace conatiner id in packs
        for (ShipmentDetails consolShipment : consolShipmentsList) {
            List<Packing> packingList = consolShipment.getPackingList();
            for (Packing packing : packingList) {
                UUID containerGuid = packingVsContainerGuid.get(packing.getGuid());
                Containers containers = uuidVsUpdatedContainers.get(containerGuid);
                if (containers != null) {
                    packing.setContainerId(containers.getId());
                }
            }
            packingRepository.saveAll(packingList);
        }














        // ShipmentSave
        // TODO: replace existing container with matching Guid of new container , same for Packs
        shipmentRepository.saveAll(consolShipmentsList);



        // ConsoleSave
//        consolidationRepository.save(console);



        return console;
    }

    public ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails, Map<UUID, UUID> packingVsContainerGuid) {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        // Container splitting and creation (With new Guids)
        // Container Attachment to shipment (add containers in shipment)
        List<ShipmentDetails> shipmentDetailsList = console.getShipmentsList().stream().toList();
        Map<UUID, List<UUID>> containerGuidToShipments = new HashMap<>();
        Map<UUID, ShipmentDetails> guidToShipment = new HashMap<>();
        shipmentDetailsList.forEach(shp -> {
            guidToShipment.put(shp.getGuid(), shp);
            Set<Containers> containersList = shp.getContainersList();

            for (Containers containers : containersList) {
                List<UUID> shipmentUuids = containerGuidToShipments.get(containers.getGuid());
                if (shipmentUuids == null) {
                    shipmentUuids = new ArrayList<>();
                }
                shipmentUuids.add(shp.getGuid());
                containerGuidToShipments.put(containers.getGuid(), shipmentUuids);
            }

            shp.setContainersList(new HashSet<>());

        });

        List<Containers> splitContainers = distributeContainers(console.getContainersList(), containerGuidToShipments);
        console.setContainersList(splitContainers);
        Map<UUID, Containers> guidVsContainer = splitContainers.stream().collect(Collectors.toMap(Containers::getGuid, Function.identity()));

        containerGuidToShipments.forEach((containerGuid, shipmentGuids) -> {
            for (UUID shipmentGuid : shipmentGuids) {
                ShipmentDetails details = guidToShipment.get(shipmentGuid);
                Containers containers = guidVsContainer.get(containerGuid);
                details.getContainersList().add(containers);
            }
        });

        if(ObjectUtils.isNotEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> {
                try {

                    shipmentMigrationV3Service.mapShipmentV2ToV3(ship, packingVsContainerGuid);
                } catch (RunnerException e) {
                    throw new IllegalArgumentException(e);
                }
            });
        }

        // Update Container from attached packages (based on packingVsContainerGuid)
        assignPackingsToContainers(shipmentDetailsList, packingVsContainerGuid, guidVsContainer);

        // Console summary update
        try {
            AchievedQuantitiesResponse achievedQuantitiesResponse = consolidationV3Service.calculateAchievedQuantities(consolidationDetails);
            AchievedQuantities achievedQuantities = jsonHelper.convertValue(achievedQuantitiesResponse, AchievedQuantities.class);
            consolidationDetails.setAchievedQuantities(achievedQuantities);
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        }

        // TODO Console to shipment cutoff fields update, Agents, etc (Refer excel)

        return console;
    }

    private void assignPackingsToContainers(List<ShipmentDetails> shipmentDetailsList, Map<UUID, UUID> packingVsContainerGuid, Map<UUID, Containers> guidVsContainer) {
        packingVsContainerGuid.forEach((packGuid, containerGuid) -> {
            Containers containers = guidVsContainer.get(containerGuid);
            containers.setGrossWeight(BigDecimal.ZERO);
            containers.setGrossWeightUnit(null);
            containers.setGrossVolume(BigDecimal.ZERO);
            containers.setGrossVolumeUnit(null);
            containers.setPacks(null);
            containers.setPacksType(null);
        });

        for (ShipmentDetails shipment : shipmentDetailsList) {
            List<Packing> packingList = shipment.getPackingList();

            if (packingList == null || packingList.isEmpty()) {
                continue;
            }

            Map<UUID, Packing> packingByGuid = packingList.stream()
                    .collect(Collectors.toMap(Packing::getGuid, Function.identity()));

            for (Map.Entry<UUID, Packing> entry : packingByGuid.entrySet()) {
                UUID packingGuid = entry.getKey();
                Packing packing = entry.getValue();

                UUID containerGuid = packingVsContainerGuid.get(packingGuid);
                Containers container = guidVsContainer.get(containerGuid);

                if (container == null) {
                    continue; // skip if container is not mapped
                }
                try {
                    // TODO: DEVA took deafault units from tenant settings | check for
                    containerV3Service.addPackageDataToContainer(container, packing);
                } catch (RunnerException e) {
                    throw new IllegalArgumentException(e);
                }
            }
        }
    }

    @Override
    public ConsolidationDetails migrateConsolidationV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }

        // Convert V3 Console and Attached shipment to V2
        ConsolidationDetails console = mapConsoleV3ToV2(consolidationDetails1.get());

        // ContainerSave
        // PackingSave
        // ShipmentSave
        // ConsoleSave

        return console;
    }

    public ConsolidationDetails mapConsoleV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        Map<String, EntityTransferContainerType> containerTypeMap = shipmentMigrationV3Service.fetchContainerTypeDetails(console.getContainersList());

        List<ShipmentDetails> shipmentDetailsList = console.getShipmentsList().stream().toList();
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
        setContainerUtilisationForConsolidation(console, shipmentDetailsList, containerTypeMap);

        //TODO : Packs utilisation Update : V3 --> V2

        return console;
    }

    public List<Containers> distributeContainers(List<Containers> inputContainers, Map<UUID, List<UUID>> containerGuidToShipments) {
        List<Containers> resultContainers = new ArrayList<>();

        for (Containers container : inputContainers) {
            Long count = container.getContainerCount();

            if (count == null || count <= 1) {
                resultContainers.add(container);
                continue;
            }

            List<Containers> tempContainers = new ArrayList<>();
            distributeMultiCountContainer(container, count, tempContainers);

            for (Containers tempContainer : tempContainers) {
                if (tempContainer.getId() == null && !containerGuidToShipments.containsKey(tempContainer.getGuid())) {
                    List<UUID> shipmentUuids = containerGuidToShipments.get(container.getGuid());
                    containerGuidToShipments.put(tempContainer.getGuid(), shipmentUuids);
                }
            }

//            List<UUID> shipmentGuids = containerGuidToShipments.get(container.getGuid());

//            shipmentGuids.forEach(sGuid -> {
//                ShipmentDetails details = guidToShipment.get(sGuid);
//                Set<Containers> containersList = details.getContainersList();
//
//                for (Containers tempContainer : tempContainers) {
//                    for (Containers containers : containersList) {
//                        if(tempContainer.getId()==null) {
//
//                        }
//                    }
//                }
//
//
//            });

            resultContainers.addAll(tempContainers);

        }

        return resultContainers;
    }

    private void distributeMultiCountContainer(Containers original, Long count, List<Containers> resultContainers) {
        BigDecimal totalWeight = safeBigDecimal(original.getGrossWeight());
        BigDecimal totalVolume = safeBigDecimal(original.getGrossVolume());

        BigDecimal[] weightParts = totalWeight.divideAndRemainder(BigDecimal.valueOf(count));
        BigDecimal[] volumeParts = totalVolume.divideAndRemainder(BigDecimal.valueOf(count));

        BigDecimal baseWeight = weightParts[0];
        BigDecimal weightRemainder = weightParts[1];
        BigDecimal baseVolume = volumeParts[0];
        BigDecimal volumeRemainder = volumeParts[1];

        // Update original container
        original.setContainerCount(1L);
        original.setGrossWeight(baseWeight);
        original.setGrossVolume(baseVolume);
        resultContainers.add(original);

        for (int i = 1; i < count; i++) {
            resultContainers.add(createDistributedCopy(original, baseWeight, baseVolume, i == count - 1 ? weightRemainder : BigDecimal.ZERO,
                    i == count - 1 ? volumeRemainder : BigDecimal.ZERO));
        }
    }

    // TODO: SUBHAM check if any more fields should be corrected - DEVA
    private Containers createDistributedCopy(Containers sourceContainer, BigDecimal baseWeight, BigDecimal baseVolume, BigDecimal weightRemainder, BigDecimal volumeRemainder) {
        Containers newContainer = jsonHelper.convertValue(sourceContainer, Containers.class);
        newContainer.setId(null);
        newContainer.setGuid(UUID.randomUUID());
        newContainer.setContainerCount(1L);
        newContainer.setGrossWeight(baseWeight.add(weightRemainder));
        newContainer.setGrossVolume(baseVolume.add(volumeRemainder));

        return newContainer;
    }

    private BigDecimal safeBigDecimal(BigDecimal value) {
        return value != null ? value : BigDecimal.ZERO;
    }


    private void setContainerUtilisationForConsolidation(ConsolidationDetails console, List<ShipmentDetails> shipmentDetailsList,
                                                         Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        //Identify container associated only with consolidation and and call setContainerUtilisationForShipment
        Set<Containers> consolContainers = getOnlyConsolidationContainers(console, shipmentDetailsList);
        boolean isFCL = Constants.CARGO_TYPE_FCL.equalsIgnoreCase(console.getShipmentType());
        shipmentMigrationV3Service.setContainerUtilisation(consolContainers, containerTypeMap, isFCL);
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

}
