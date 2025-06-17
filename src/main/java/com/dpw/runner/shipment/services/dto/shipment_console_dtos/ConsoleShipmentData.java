package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import lombok.Data;

@Data
public class ConsoleShipmentData extends ShipmentWtVolResponse {
    boolean isCreate;
    boolean syncConsole;
    boolean isFromET;
    ConsolidationDetails consolidationDetails;
}
