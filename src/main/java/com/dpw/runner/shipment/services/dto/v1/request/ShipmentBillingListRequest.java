package com.dpw.runner.shipment.services.dto.v1.request;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
@Builder
public class ShipmentBillingListRequest {
    private List<UUID> guidsList;
}
