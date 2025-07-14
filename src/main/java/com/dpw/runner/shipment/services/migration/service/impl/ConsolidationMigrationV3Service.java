package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
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
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.*;
import java.util.stream.Collectors;

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
    IContainerRepository containerRepository;
    @Autowired
    IPackingRepository packingRepository;
    @Autowired
    IShipmentRepository shipmentRepository;
    @Autowired
    IConsolidationRepository consolidationRepository;

    @Autowired
    private IPackingService packingService;

    @Override
    @Transactional
    public ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails) {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }

        // Convert V2 Console and Attached shipment to V3
        ConsolidationDetails console = mapConsoleV2ToV3(consolidationDetails1.get());

        // ContainerSave
        // Attach Container to shipment
        // PackingSave
        // ShipmentSave
        // ConsoleSave


        return console;
    }

    public ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails) {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        // Container splitting and creation (With new Guids)

        // Container Attachment to shipment (add containers in shipment)


        List<ShipmentDetails> shipmentDetailsList = console.getShipmentsList().stream().toList();
        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> {
                try {
                    shipmentMigrationV3Service.mapShipmentV2ToV3(ship, packingVsContainerGuid);
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            });
        }

        // Update Container from attached packages (based on packingVsContainerGuid)

        // Console summary update

        // Console to shipment cutoff fields update, Agents, etc (Refer excel)

        return console;
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
        containerRepository.saveAll(console.getContainersList());
        // PackingSave
        packingRepository.saveAll(console.getPackingList());
        // ShipmentSave
        shipmentRepository.saveAll(console.getShipmentsList());
        // ConsoleSave
        consolidationRepository.save(console);

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
        packingService.calculatePacksUtilisationForConsolidation(calculatePackUtilizationRequest);
    }

    private Set<Containers> setContainerUtilisationForConsolidation(ConsolidationDetails console, List<ShipmentDetails> shipmentDetailsList,
                                                         Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        //Identify container associated only with consolidation and and call setContainerUtilisationForShipment
        Set<Containers> consolContainers = getOnlyConsolidationContainers(console, shipmentDetailsList);
        boolean isFCL = Constants.CARGO_TYPE_FCL.equalsIgnoreCase(console.getShipmentType());
        shipmentMigrationV3Service.setContainerUtilisation(consolContainers, containerTypeMap, isFCL);
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

}
