package com.dpw.runner.shipment.services.validator.annotations;

import com.dpw.runner.shipment.services.validator.custom.validations.ContainerRequestValidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ContainerRequestValidator.class)
public @interface ValidContainerRequest {
    String message() default "Invalid container request";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
