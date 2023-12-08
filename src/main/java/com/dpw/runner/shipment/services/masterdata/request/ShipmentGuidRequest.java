package com.dpw.runner.shipment.services.masterdata.request;

import lombok.Data;

import java.util.UUID;

@Data
public class ShipmentGuidRequest {
    private UUID shipmentGuid;
}
