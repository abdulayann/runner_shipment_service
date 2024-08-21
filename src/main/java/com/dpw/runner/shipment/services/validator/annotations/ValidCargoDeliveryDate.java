package com.dpw.runner.shipment.services.validator.annotations;

import com.dpw.runner.shipment.services.validator.custom.validations.CargoDeliveryDateValidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Constraint(validatedBy = CargoDeliveryDateValidator.class)
@Target({ ElementType.FIELD })
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidCargoDeliveryDate {
    String message() default "Cargo Delivery Date should not be lesser than Cargo Ready Date.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
