package com.dpw.runner.shipment.services.validator.custom.validations;

import jakarta.validation.ConstraintValidatorContext;

public class TICommonValidator {
    private TICommonValidator() {

    }

    public static void dgClassValidation(ConstraintValidatorContext context) {
        context.buildConstraintViolationWithTemplate("DG Class is mandatory when DG is enabled")
                .addPropertyNode("dgClass")
                .addConstraintViolation();
    }

    public static void unNumberValidation(ConstraintValidatorContext context) {
        context.buildConstraintViolationWithTemplate("UN Number is mandatory when DG is enabled")
                .addPropertyNode("unNumber")
                .addConstraintViolation();
    }

    public static void properShippingLineValidation(ConstraintValidatorContext context) {
        context.buildConstraintViolationWithTemplate("Proper Shipping Name is mandatory when DG is enabled")
                .addPropertyNode("properShippingName")
                .addConstraintViolation();
    }

    public static void dgClassDescriptionValidation(ConstraintValidatorContext context) {
        context.buildConstraintViolationWithTemplate("DG Class Description is mandatory when DG is enabled")
                .addPropertyNode("dgClassDescription")
                .addConstraintViolation();
    }

    public static void hazardLabelValidation(ConstraintValidatorContext context) {
        context.buildConstraintViolationWithTemplate("DG Class is mandatory when DG is enabled")
                .addPropertyNode("hazardLabel")
                .addConstraintViolation();
    }
}
