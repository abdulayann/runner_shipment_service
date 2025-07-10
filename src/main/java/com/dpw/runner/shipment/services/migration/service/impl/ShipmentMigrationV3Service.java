package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.UUID;

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
    private CommonUtils commonUtils;
    @Autowired
    private IPackingV3Service packingV3Service;

    @Override
    public ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) throws RunnerException {

        mapShipmentV2ToV3(shipmentDetails, packingVsContainerGuid);

        return null;
    }

    public ShipmentDetails mapShipmentV2ToV3(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) throws RunnerException {
        // Business Logic for transformation

        // Update Packs based on Auto Update Weight Volume flag;
        transformContainerAndPacks(shipmentDetails);

        // Update Shipment Summary
        updateShipmentCargoSummary(shipmentDetails);


        return shipmentDetails;
    }

    private void transformContainerAndPacks(ShipmentDetails shipmentDetails) {
        if(Boolean.TRUE.equals(shipmentDetails.getAutoUpdateWtVol())) {


        } else {
            if(!CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                redistributeSummaryToPacks(shipmentDetails);
            } else if (CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                createPacksForContainerLineItems(shipmentDetails);
            } else if (!CommonUtils.listIsNullOrEmpty(shipmentDetails.getPackingList()) && !CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
                removeExistingPacksAndCreateNewPacks(shipmentDetails);
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
        String volumeUnit = shipmentDetails.getVolumeUnit() != null? shipmentDetails.getVolumeUnit() : commonUtils.getDefaultVolumeUnit(); // Default Pending
        String weightUnit = shipmentDetails.getWeightUnit() != null? shipmentDetails.getWeightUnit() : commonUtils.getDefaultWeightUnit();  // Default Pending
        String packsType = shipmentDetails.getPacksUnit() != null? shipmentDetails.getPacksUnit() : PackingConstants.PKG;  // Default Pending

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

    private void createPacksForContainerLineItems(ShipmentDetails shipmentDetails) {
        int totalContainerCount = shipmentDetails.getContainersList().stream().filter(x->x.getContainerCount() != null)
                .mapToInt(x->x.getContainerCount().intValue()).sum();
        shipmentDetails.setPackingList(new ArrayList<>());
        for (int i = 0; i < totalContainerCount; i++) {
            Packing packing = new Packing();
            packing.setGuid(UUID.randomUUID());
            shipmentDetails.getPackingList().add(packing);
        }
        shipmentDetails.setNoOfPacks(totalContainerCount);
        redistributeSummaryToPacks(shipmentDetails);
    }

    private void removeExistingPacksAndCreateNewPacks(ShipmentDetails shipmentDetails) {
        // Remove Existing Packs
        shipmentDetails.setPackingList(new ArrayList<>());
        createPacksForContainerLineItems(shipmentDetails);
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
        mapShipmentV3ToV2(shipmentDetails);

        return shipmentDetails;
    }

    public ShipmentDetails mapShipmentV3ToV2(ShipmentDetails shipmentDetails) throws RunnerException {
        // Business Logic for transformation
        // need to add shipment details transformation logic
        shipmentDetails.setAutoUpdateWtVol(false);
        shipmentDetails.setContainerAutoWeightVolumeUpdate(false);

        // update container utilisation
        setContainerUtilisation(shipmentDetails);

        return shipmentDetails;
    }

    public void setContainerUtilisation(ShipmentDetails shipmentDetails) throws RunnerException {
        if (CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            return;
        }
        boolean isFCL = Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType());
        Map<String, EntityTransferContainerType> containerTypeMap = fetchContainerTypeDetails(shipmentDetails);
        for (Containers container: shipmentDetails.getContainersList()) {
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

    public Map<String, EntityTransferContainerType> fetchContainerTypeDetails(ShipmentDetails shipmentDetails) {
        Set<String> containerTypeCodes = new HashSet<>();
        shipmentDetails.getContainersList().forEach(containers -> containerTypeCodes.add(containers.getContainerCode()));
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
