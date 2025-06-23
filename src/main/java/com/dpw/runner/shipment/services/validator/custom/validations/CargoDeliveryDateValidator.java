package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.validator.annotations.ValidCargoDeliveryDate;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.time.LocalDateTime;

public class CargoDeliveryDateValidator implements ConstraintValidator<ValidCargoDeliveryDate, ShipmentRequest> {

    @Override
    public boolean isValid(ShipmentRequest shipment, ConstraintValidatorContext context) {
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
