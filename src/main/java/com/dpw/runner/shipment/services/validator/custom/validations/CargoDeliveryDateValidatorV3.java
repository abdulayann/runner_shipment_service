package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.validator.annotations.ValidCargoDeliveryDate;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.time.LocalDateTime;

public class CargoDeliveryDateValidatorV3 implements ConstraintValidator<ValidCargoDeliveryDate, ShipmentV3Request> {

    @Override
    public boolean isValid(ShipmentV3Request shipment, ConstraintValidatorContext context) {
        LocalDateTime cargoReadyDate = shipment.getCargoReadyDate();
        LocalDateTime cargoDeliveryDate = shipment.getCargoDeliveryDate();

        if (cargoDeliveryDate != null && cargoReadyDate != null && cargoDeliveryDate.isBefore(cargoReadyDate)) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate(
                    "Cargo Delivery Date should not be lesser than Cargo Ready Date."
                ).addPropertyNode("cargoDeliveryDate").addConstraintViolation();
                return false;
        }
        return true;
    }
}
