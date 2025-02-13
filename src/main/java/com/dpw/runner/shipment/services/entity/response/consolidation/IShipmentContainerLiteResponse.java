package com.dpw.runner.shipment.services.entity.response.consolidation;

public interface IShipmentContainerLiteResponse extends IShipmentLiteResponse, IContainerLiteResponse {
    Long getShipId();
}
