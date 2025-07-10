package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
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
    private IContainerV3Service containerV3Service;

    @Autowired
    private IConsolidationV3Service consolidationV3Service;

    @Override
    @Transactional
    public ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails) {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }

        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        // Convert V2 Console and Attached shipment to V3
        ConsolidationDetails console = mapConsoleV2ToV3(consolidationDetails1.get(), packingVsContainerGuid);

        // ContainerSave
        //TODO guid to container -> shipment's container store


        // Attach Container to shipment

        // PackingSave
        // TODO: replace conatiner id in packs

        // ShipmentSave
        // TODO: replace existing container with matching Guid of new container , same for Packs


        // ConsoleSave


        return console;
    }

    public ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails, Map<UUID, UUID> packingVsContainerGuid) {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        // Container splitting and creation (With new Guids)
        // Container Attachment to shipment (add containers in shipment)
        List<Containers> splitContainers = distributeContainers(console.getContainersList());
        Map<UUID, Containers> guidVsContainer = splitContainers.stream().collect(Collectors.toMap(Containers::getGuid, Function.identity()));

        List<ShipmentDetails> shipmentDetailsList = console.getShipmentsList().stream().toList();

        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> {
                try {
                    shipmentMigrationV3Service.migrateShipmentV2ToV3(ship, packingVsContainerGuid);
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            });
        }

        // Update Container from attached packages (based on packingVsContainerGuid)
        assignPackingsToContainers(shipmentDetailsList, packingVsContainerGuid, guidVsContainer);

        // Console summary update
        try {
            consolidationV3Service.getConsoleSyncAchievedData(consolidationDetails.getId());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        // Console to shipment cutoff fields update, Agents, etc (Refer excel)

        return console;
    }

    private void assignPackingsToContainers(List<ShipmentDetails> shipmentDetailsList, Map<UUID, UUID> packingVsContainerGuid, Map<UUID, Containers> guidVsContainer) {
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

                // TODO update container data from Packs

//                container.setPacksList(
//                        Optional.ofNullable(container.getPacksList())
//                                .orElseGet(ArrayList::new)
//                );
//
//                container.getPacksList().add(packing);


                try {
                    containerV3Service.addPackageDataToContainer(container, packing);// TODO check for PACK LIST
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    @Override
    public ConsolidationDetails migrateConsolidationV3ToV2(ConsolidationDetails consolidationDetails) {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }
        List<ShipmentDetails> shipmentDetailsList = consolidationDetails1.get().getShipmentsList().stream().toList();
        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> shipmentMigrationV3Service.migrateShipmentV3ToV2(ship));
        }

        return consolidationDetails1.get();
    }

    public List<Containers> distributeContainers(List<Containers> inputContainers) {
        List<Containers> resultContainers = new ArrayList<>();

        for (Containers container : inputContainers) {
            Long count = container.getContainerCount();

            if (count == null || count <= 1) {
                resultContainers.add(container);
                continue;
            }

            distributeMultiCountContainer(container, count, resultContainers);
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

        // TODO: SUBHAM handle FCL / LCL
        Set<ShipmentDetails> shipmentsList = sourceContainer.getShipmentsList();
        newContainer.setShipmentsList(shipmentsList);
//
//        for (ShipmentDetails details : shipmentsList) {
//            details.getContainersList().add(newContainer);
//            details.getContainersList().add(sourceContainer);
//        }



        return newContainer;
    }

    private BigDecimal safeBigDecimal(BigDecimal value) {
        return value != null ? value : BigDecimal.ZERO;
    }

}
