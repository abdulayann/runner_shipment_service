package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class CarrierBookingValidationUtil {

    @Autowired
    private IConsolidationV3Service consolidationV3Service;

    public void validateRequest(CarrierBookingRequest request) {

    }
}
