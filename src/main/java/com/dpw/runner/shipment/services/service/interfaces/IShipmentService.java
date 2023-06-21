package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IShipmentService {
    List<ShipmentDetails> createTestShipment(Integer count);
    ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel);

    ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> list(CommonRequestModel commonRequestModel);

    ResponseEntity<?> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);
}