package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.validator.annotations.MaxTotalDigits;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.math.BigDecimal;

public class MaxTotalDigitsValidator implements ConstraintValidator<MaxTotalDigits, BigDecimal> {

    private int maxDigits;

    @Override
    public void initialize(MaxTotalDigits constraintAnnotation) {
        this.maxDigits = constraintAnnotation.value();
    }

    @Override
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        if (value == null) return true; // allow nulls — use @NotNull separately if needed

        // Get plain string representation (e.g., "24500000000000000.12300")
        String plainStr = value.toPlainString();

        // Remove sign
        plainStr = plainStr.replace("-", "");

        // Get the part before the decimal
        String integerPart = plainStr.contains(".") ? plainStr.split("\\.")[0] : plainStr;

        // Count digits before the decimal point
        return integerPart.length() <= maxDigits;
    }
}

