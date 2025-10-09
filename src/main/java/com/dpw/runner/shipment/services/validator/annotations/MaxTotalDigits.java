package com.dpw.runner.shipment.services.validator.annotations;

import com.dpw.runner.shipment.services.validator.custom.validations.MaxTotalDigitsValidator;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

@Documented
@Constraint(validatedBy = MaxTotalDigitsValidator.class)
@Target({ ElementType.FIELD })
@Retention(RetentionPolicy.RUNTIME)
public @interface MaxTotalDigits {
    String message() default "Digit length must not exceed {value} total digits.";
    int value();
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
