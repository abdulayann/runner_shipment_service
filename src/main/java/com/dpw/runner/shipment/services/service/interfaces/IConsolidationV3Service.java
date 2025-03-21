package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;

public interface IConsolidationV3Service {

    String attachShipments(ShipmentAttachDetachV3Request shipmentAttachDetachRequest);
}