package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
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

    @Override
    public ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails) {


        mapShipmentV2ToV3(shipmentDetails);

        // update Shipment

        return null;
    }

    public ShipmentDetails mapShipmentV2ToV3(ShipmentDetails shipmentDetails) {
        // Business Logic for transformation

        return shipmentDetails;
    }

    @Override
    public ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails) {
        mapShipmentV3ToV2(shipmentDetails);

        // update Shipment
        return shipmentDetails;
    }

    public ShipmentDetails mapShipmentV3ToV2(ShipmentDetails shipmentDetails) {
        // Business Logic for transformation
        // need to add shipment details transformation logic

        // update container utilisation
        try {
            setContainerUtilisation(shipmentDetails);
        } catch (RunnerException ex) {
            log.error(ex.getMessage());
        }

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
                containerTypesList.forEach(entityTransferContainerType -> {
                    containerTypeMap.put(entityTransferContainerType.Code, entityTransferContainerType);
                });
            }
        }
        return containerTypeMap;
    }
}
