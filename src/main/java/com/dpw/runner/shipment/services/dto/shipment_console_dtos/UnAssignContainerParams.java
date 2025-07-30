package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.entity.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UnAssignContainerParams {
    Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
    Map<Long, List<Packing>> shipmentPackingMap = new HashMap<>();
    List<ShipmentsContainersMapping> shipmentsContainersMappings = new ArrayList<>();
    Set<Long> fclOrFtlShipmentIds = new HashSet<>();
    ShipmentWtVolResponse oldShipmentWtVolResponse = null;
    ConsolidationDetails consolidationDetails = null;
    Long consolidationId;
    List<Long> shipmentIdsForCargoDetachment = new ArrayList<>();
    List<Long> removeAllPackingIds = new ArrayList<>();
    Containers oldContainersEntity = new Containers();
}
