package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import lombok.Data;

@Data
public class ContainerBeforeSaveRequest {
    ConsolidationDetails consolidationDetails;
    ShipmentWtVolResponse shipmentWtVolResponse;
}
