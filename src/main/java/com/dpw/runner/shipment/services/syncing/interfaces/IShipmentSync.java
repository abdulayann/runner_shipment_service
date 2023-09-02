package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentRequest;

public interface IShipmentSync {
    CustomShipmentRequest sync(ShipmentDetails shipmentDetails);
    ShipmentDetails reverseSync(CustomShipmentRequest cs);
}
