package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class TransportInstructionValidationUtil {
    @Autowired
    @Lazy
    private IShipmentServiceV3 shipmentServiceV3;

    public ShipmentDetails validateShipmentId(PickupDeliveryDetailsRequest request) {
        if (request.getShipmentId() == null) {
            throw new ValidationException("ShipmentId is required to create transport instruction");
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentServiceV3.findById(request.getShipmentId());
        if (!shipmentDetails.isPresent()) {
            throw new ValidationException("Please provide valid ShipmentId");
        }
        return shipmentDetails.get();
    }
}
