package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public interface IShipmentMigrationV3Service {
    ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails) throws RunnerException;
    ShipmentDetails mapShipmentV2ToV3(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) throws RunnerException;

    ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails) throws RunnerException;
    ShipmentDetails mapShipmentV3ToV2(ShipmentDetails shipmentDetails, Map<String, EntityTransferContainerType> containerTypeMap) throws RunnerException;
    void setContainerUtilisation(Set<Containers> containers, Map<String, EntityTransferContainerType> containerTypeMap, boolean isFCL) throws RunnerException;

    Map<String, EntityTransferContainerType> fetchContainerTypeDetails(List<Containers> containers);
}
