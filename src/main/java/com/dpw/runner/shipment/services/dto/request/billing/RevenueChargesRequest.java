package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class RevenueChargesRequest {
    private UUID shipmentGuid;
}
