package com.dpw.runner.shipment.services.validator.annotations;

import com.dpw.runner.shipment.services.validator.custom.validations.PackageRequestValidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = PackageRequestValidator.class)
public @interface ValidPackageRequest {
    String message() default "Invalid package request";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}