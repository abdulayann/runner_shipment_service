package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Slf4j
@Component
public class VerifiedGrossMassValidationUtil {

    @Autowired
    private IConsolidationV3Service consolidationV3Service;
    @Autowired
    private ICarrierBookingService carrierBookingService;

    public Object validateRequest(EntityType entityType, Long entityId) {
        if (EntityType.CONSOLIDATION.equals(entityType)) {
            return consolidationV3Service.findById(entityId).orElseThrow(() -> new ValidationException("Invalid Consolidation id"));
        } else if (EntityType.CARRIER_BOOKING.equals(entityType)) {
            return carrierBookingService.findById(entityId).orElseThrow(() -> new ValidationException("Invalid Carrier booking id"));
        }
        throw new ValidationException("Invalid entity Type");
    }

    public void validateServiceType(VerifiedGrossMassRequest request) {
        if (Objects.isNull(request.getServiceType()) || !VerifiedGrossMassConstants.serviceTypes.contains(request.getServiceType())) {
            throw new ValidationException("Unsupported service type. Please select one of: P2P,D2D,D2P,P2D");
        }
    }

}
