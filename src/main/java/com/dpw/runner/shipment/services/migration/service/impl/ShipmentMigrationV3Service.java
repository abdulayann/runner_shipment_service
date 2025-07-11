package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
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
    IPackingDao packingDao;

    @Autowired
    IContainerDao containerDao;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IPackingV3Service packingV3Service;

    @Override
    public ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails) throws RunnerException {
        // Handle migration of all the shipments where there is no console attached.
        Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findById(shipmentDetails.getId());
        if(shipmentDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Shipment found with given id: " + shipmentDetails.getId());
        }
        ShipmentDetails shipment = jsonHelper.convertValue(shipmentDetails1.get(), ShipmentDetails.class);
        mapShipmentV2ToV3(shipment, new HashMap<>());

        // Save packing details
        packingDao.saveAll(shipment.getPackingList());
        // save shipment
        shipmentRepository.save(shipment);
        return shipment;
    }

    @Override
    public ShipmentDetails mapShipmentV2ToV3(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) throws RunnerException {
        // Business Logic for transformation

        // Update Packs based on Auto Update Weight Volume flag
        transformContainerAndPacks(shipmentDetails, packingVsContainerGuid);

        // Update Shipment Summary
        updateShipmentCargoSummary(shipmentDetails);

        return shipmentDetails;
    }

    private void transformContainerAndPacks(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        if(Boolean.TRUE.equals(shipmentDetails.getAutoUpdateWtVol())) {
            if(CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                createPacksWithContainerSummary(shipmentDetails, packingVsContainerGuid);
            } else if (!CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                createPacksForUnassignedContainers(shipmentDetails, packingVsContainerGuid);
            }

        } else {
            if(!CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                redistributeSummaryToPacks(shipmentDetails);
            } else if (CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                createPacksForContainerLineItems(shipmentDetails, packingVsContainerGuid);
            } else if (!CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                removeExistingPacksAndCreateNewPacks(shipmentDetails, packingVsContainerGuid);
            }
        }
    }

    private void redistributeSummaryToPacks(ShipmentDetails shipmentDetails) {
        int packLineItems = shipmentDetails.getPackingList().size();
        int weight = shipmentDetails.getWeight().divide(BigDecimal.valueOf(packLineItems),10, RoundingMode.DOWN).intValue();
        int volume = shipmentDetails.getVolume().divide(BigDecimal.valueOf(packLineItems),10, RoundingMode.DOWN).intValue();
        Integer noOfPacks = shipmentDetails.getNoOfPacks()/packLineItems;
        if(noOfPacks == 0) {
            noOfPacks = 1;
        }
        String volumeUnit = !CommonUtils.isStringNullOrEmpty(shipmentDetails.getVolumeUnit())? shipmentDetails.getVolumeUnit() : commonUtils.getDefaultVolumeUnit();
        String weightUnit = !CommonUtils.isStringNullOrEmpty(shipmentDetails.getWeightUnit())? shipmentDetails.getWeightUnit() : commonUtils.getDefaultWeightUnit();
        String packsType = commonUtils.getPacksUnit(shipmentDetails.getPacksUnit());

        for (int i = 0; i < packLineItems; i++) {
            if(i == packLineItems-1) {
                shipmentDetails.getPackingList().get(i).setWeight(shipmentDetails.getWeight().subtract(BigDecimal.valueOf(weight)));
                shipmentDetails.getPackingList().get(i).setVolume(shipmentDetails.getVolume().subtract(BigDecimal.valueOf(volume)));
                shipmentDetails.getPackingList().get(i).setPacks(String.valueOf((shipmentDetails.getNoOfPacks() - noOfPacks) > 0 ? (shipmentDetails.getNoOfPacks() - noOfPacks) : 1));
            } else {
                shipmentDetails.getPackingList().get(i).setWeight(BigDecimal.valueOf(weight));
                shipmentDetails.getPackingList().get(i).setVolume(BigDecimal.valueOf(volume));
                shipmentDetails.getPackingList().get(i).setPacks(String.valueOf(noOfPacks));
            }
            shipmentDetails.getPackingList().get(i).setWeightUnit(weightUnit);
            shipmentDetails.getPackingList().get(i).setVolumeUnit(volumeUnit);
            shipmentDetails.getPackingList().get(i).setPacksType(packsType);
        }
    }

    private void createPacksForContainerLineItems(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        if(shipmentDetails.getPackingList() == null)
            shipmentDetails.setPackingList(new ArrayList<>());
        shipmentDetails.getContainersList().forEach(container -> {
            Packing packing = new Packing();
            packing.setGuid(UUID.randomUUID());
            packing.setShipmentId(shipmentDetails.getId());
            packingVsContainerGuid.putIfAbsent(packing.getGuid(), container.getGuid());
            shipmentDetails.getPackingList().add(packing);
        });

        // Distribute summary to packs equally
        redistributeSummaryToPacks(shipmentDetails);
    }

    private void removeExistingPacksAndCreateNewPacks(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        // Delete Existing Packs
        shipmentDetails.getPackingList().forEach(packing -> packing.setIsDeleted(true));
        createPacksForContainerLineItems(shipmentDetails, packingVsContainerGuid);
    }

    private void createPacksWithContainerSummary(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        shipmentDetails.setPackingList(new ArrayList<>());
        shipmentDetails.getContainersList().forEach(container -> {
            Packing packing = new Packing();
            packing.setGuid(UUID.randomUUID());
            packingVsContainerGuid.putIfAbsent(packing.getGuid(), container.getGuid());
            // populate Container info to pack
            createPackWithContainerInfo(packing, container);
            shipmentDetails.getPackingList().add(packing);
        });
    }

    private void createPackWithContainerInfo(Packing packing, Containers container) {
        String count = !isStringNullOrEmpty(container.getPacks()) ? container.getPacks() : "1";
        packing.setPacks(count);
        packing.setPacks(commonUtils.getPacksUnit(container.getPacksType()));
        packing.setWeight(container.getGrossWeight());
        packing.setWeightUnit(!CommonUtils.isStringNullOrEmpty(container.getGrossWeightUnit()) ? container.getGrossWeightUnit() : commonUtils.getDefaultWeightUnit());
        packing.setVolume(container.getGrossVolume());
        packing.setVolumeUnit(!CommonUtils.isStringNullOrEmpty(container.getGrossVolumeUnit()) ? container.getGrossVolumeUnit() : commonUtils.getDefaultVolumeUnit());
        packing.setCommodity(container.getCommodityCode());
    }

    private void createPacksForUnassignedContainers(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) {
        Map<Long, Containers> containerMap = shipmentDetails.getContainersList().stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));
        shipmentDetails.getPackingList().forEach(packing -> containerMap.remove(packing.getContainerId()));
        if(!containerMap.isEmpty()) {
            for (var cont: containerMap.values()) {
                Packing packing = new Packing();
                packing.setGuid(UUID.randomUUID());
                packing.setShipmentId(shipmentDetails.getId());
                packingVsContainerGuid.putIfAbsent(packing.getGuid(), cont.getGuid());
                // populate Container info to pack
                createPackWithContainerInfo(packing, cont);
                shipmentDetails.getPackingList().add(packing);
            }
        }
    }

    private void updateShipmentCargoSummary(ShipmentDetails shipmentDetails) throws RunnerException {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setWeight(shipmentDetails.getWeight());
        cargoDetailsResponse.setWeightUnit(shipmentDetails.getWeightUnit());
        cargoDetailsResponse.setTransportMode(shipmentDetails.getTransportMode());
        cargoDetailsResponse.setShipmentType(shipmentDetails.getShipmentType());
        cargoDetailsResponse = packingV3Service.calculateCargoDetails(shipmentDetails.getPackingList(), cargoDetailsResponse);

        // update to shipment fields
        shipmentDetails.setNoOfPacks(cargoDetailsResponse.getNoOfPacks());
        shipmentDetails.setPacksUnit(cargoDetailsResponse.getPacksUnit());
        shipmentDetails.setVolume(cargoDetailsResponse.getVolume());
        shipmentDetails.setVolumeUnit(cargoDetailsResponse.getVolumeUnit());
        shipmentDetails.setWeight(cargoDetailsResponse.getWeight());
        shipmentDetails.setWeightUnit(cargoDetailsResponse.getWeightUnit());
        shipmentDetails.setVolumetricWeight(cargoDetailsResponse.getVolumetricWeight());
        shipmentDetails.setVolumetricWeightUnit(cargoDetailsResponse.getVolumetricWeightUnit());
        shipmentDetails.setChargable(cargoDetailsResponse.getChargable());
        shipmentDetails.setChargeableUnit(cargoDetailsResponse.getChargeableUnit());
        shipmentDetails.setDgPacksCount(cargoDetailsResponse.getDgPacks());
        shipmentDetails.setDgPacksUnit(cargoDetailsResponse.getDgPacksUnit());
    }

    @Override
    public ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails) throws RunnerException {
        Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findById(shipmentDetails.getId());
        if(shipmentDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Shipment found with given id: " + shipmentDetails.getId());
        }
        ShipmentDetails shipment = jsonHelper.convertValue(shipmentDetails1.get(), ShipmentDetails.class);
        Map<String, EntityTransferContainerType> containerTypeMap = fetchContainerTypeDetails(shipmentDetails.getContainersList());
        mapShipmentV3ToV2(shipment, containerTypeMap);

        if (!CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            containerDao.saveAll(shipmentDetails.getContainersList().stream().toList());
        }
        // save shipment
        shipmentRepository.save(shipment);
        return shipment;
    }

    @Override
    public ShipmentDetails mapShipmentV3ToV2(ShipmentDetails shipmentDetails, Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        // Business Logic for transformation
        // need to add shipment details transformation logic
        shipmentDetails.setAutoUpdateWtVol(false);
        shipmentDetails.setContainerAutoWeightVolumeUpdate(false);
        shipmentDetails.setCargoReadyDate(shipmentDetails.getCargoReadinessDate());

        // update container utilisation
        setContainerUtilisationForShipment(shipmentDetails, containerTypeMap);

        return shipmentDetails;
    }

    public void setContainerUtilisationForShipment(ShipmentDetails shipmentDetails, Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException {
        if (CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            return;
        }
        boolean isFCL = Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType());
        setContainerUtilisation(shipmentDetails.getContainersList(), containerTypeMap, isFCL);
    }

    public void setContainerUtilisation(Set<Containers> containers, Map<String, EntityTransferContainerType> containerTypeMap, boolean isFCL) throws RunnerException {
        for (Containers container: containers) {
            container.setIsAttached(true);
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
        if(containers.getGrossWeight() != null && !isStringNullOrEmpty(containers.getGrossWeightUnit())
                && !isStringNullOrEmpty(containerType.getMaxCargoGrossWeightUnit()) && containerType.getMaxCargoGrossWeight() != null) {
            Double weight = (Double) convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), containerType.getMaxCargoGrossWeightUnit());
            containers.setAchievedWeight(BigDecimal.valueOf(weight));
        }
        if(containers.getGrossVolume() != null && !isStringNullOrEmpty(containers.getGrossVolumeUnit())
                && !isStringNullOrEmpty(containerType.getCubicCapacityUnit()) && containerType.getCubicCapacity() != null) {
            Double volume = (Double) convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), containerType.getCubicCapacityUnit());
            containers.setAchievedVolume(BigDecimal.valueOf(volume));
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
                containerTypeCodes.stream().toList()
        );
        listRequest.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(listRequest);
        Map<String, EntityTransferContainerType> containerTypeMap = new HashMap<>();
        if(v1DataResponse != null && v1DataResponse.entities != null) {
            List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferContainerType.class);
            if(!containerTypesList.isEmpty()) {
                containerTypesList.forEach(entityTransferContainerType -> containerTypeMap.put(entityTransferContainerType.Code, entityTransferContainerType));
            }
        }
        return containerTypeMap;
    }
}
