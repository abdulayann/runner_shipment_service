package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RevenueChargesRequest {
    private Long shipmentGuid;
}
