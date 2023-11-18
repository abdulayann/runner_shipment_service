package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

public interface IOrderManagementAdapter {

    ShipmentDetails getOrder(String orderId);
}
