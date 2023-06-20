package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IShipmentService {
    List<ShipmentDetails> createTestShipment(Integer count);
    ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel);
}