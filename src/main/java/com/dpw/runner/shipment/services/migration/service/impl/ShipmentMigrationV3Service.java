package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Map;
import java.util.UUID;

@Slf4j
@Service
public class ShipmentMigrationV3Service implements IShipmentMigrationV3Service {

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
    public ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails) {
        return null;
    }
}
