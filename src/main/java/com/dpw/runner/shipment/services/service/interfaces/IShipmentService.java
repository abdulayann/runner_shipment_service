package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.Pageable;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public interface IShipmentService {
    List<ShipmentDetails> createTestShipment(Integer count);
    RunnerResponse fetchShipments(Pageable pageable);
}