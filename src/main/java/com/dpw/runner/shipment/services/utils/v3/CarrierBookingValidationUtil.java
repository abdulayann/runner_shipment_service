package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class CarrierBookingValidationUtil {

    @Autowired
    private IConsolidationV3Service consolidationV3Service;

    public Object validateRequest(String entityType, Long entityId) {
        if (Constants.CONSOLIDATION.equalsIgnoreCase(entityType)) {
            return consolidationV3Service.findById(entityId).orElseThrow(() -> new ValidationException("Invalid Consolidation id"));
        }
        throw new ValidationException("Invalid entity Type");
    }

    public void validateServiceType(CarrierBookingRequest request) {
        if (!CarrierBookingConstants.serviceTypes.contains(request.getServiceType())) {
            throw new ValidationException("Unsupported service type. Please select one of: P2P, F2P, P2F, F2F.");
        }
    }

}
