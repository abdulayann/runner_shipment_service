package com.dpw.runner.shipment.services.dto.response.OrderManagement;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CheckDGShipmentV3 {
    private Boolean isDGShipmentPresent;
    private Boolean isAnyContainerDG;
}
