package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
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
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    ConsolidationDetails consolidationDetails;
}
