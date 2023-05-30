package com.dpw.runner.shipment.services.service;

import com.dpw.runner.shipment.services.dto.request.Pageable;
import com.dpw.runner.shipment.services.dto.response.RunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public interface IShipmentService {
    List<ShipmentDetails> createTestShipment(Integer count);
    RunnerResponse fetchShipments(Pageable pageable);
}