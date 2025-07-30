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
public class AssignContainerParams {
    Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
    Map<Long, Packing> packingListMap = new HashMap<>();
    List<Packing> assignedPacks = new ArrayList<>();
    List<ShipmentsContainersMapping> shipmentsContainersMappings = new ArrayList<>();
    Set<Long> assignedShipIds = new HashSet<>();
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    Set<Long> fclOrFtlShipmentIds = new HashSet<>();
    ConsolidationDetails consolidationDetails;
    Long consolidationId;
    List<Long> shipmentIdsToSetContainerCargo = new ArrayList<>();
    List<Long> shipmentIdsToRemoveContainerCargo = new ArrayList<>();
}
