package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.validator.annotations.ValidPackageRequest;
import org.springframework.util.StringUtils;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class PackageRequestValidator implements ConstraintValidator<ValidPackageRequest, TransportInstructionLegsPackagesRequest> {

    @Override
    public boolean isValid(TransportInstructionLegsPackagesRequest request, ConstraintValidatorContext context) {
        if (request == null) {
            return true; // Let @NotNull handle null validation
        }

        boolean isValid = true;
        context.disableDefaultConstraintViolation();

        // Determine mode from context or pass as parameter
        String mode = getTransportMode(request); // You'll need to implement this method
        Boolean isDangerous = request.getDangerous();

        if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(mode)) {
            isValid = validateSeaPackageRequest(request, context, isDangerous);
        } else if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(mode)) {
            isValid = validateAirPackageRequest(request, context, isDangerous);
        }

        return isValid;
    }

    private boolean validateSeaPackageRequest(TransportInstructionLegsPackagesRequest request,
                                              ConstraintValidatorContext context, Boolean isDangerous) {
        boolean isValid = true;

        if (Boolean.TRUE.equals(isDangerous)) {
            // Order 2: DG Class - Mandatory when DG is On (using hazardLabel field)
            if (!StringUtils.hasText(request.getHazardLabel())) {
                TICommonValidator.hazardLabelValidation(context);
                isValid = false;
            }

            // Order 3: UN Number - Mandatory when DG is On
            if (!StringUtils.hasText(request.getUnNumber())) {
                TICommonValidator.unNumberValidation(context);
                isValid = false;
            }

            // Order 4: Proper Shipping Name - Mandatory when DG is On
            if (!StringUtils.hasText(request.getProperShippingName())) {
                TICommonValidator.properShippingLineValidation(context);
                isValid = false;
            }
        }

        return isValid;
    }

    private boolean validateAirPackageRequest(TransportInstructionLegsPackagesRequest request,
                                              ConstraintValidatorContext context, Boolean isDangerous) {
        boolean isValid = true;

        if (Boolean.TRUE.equals(isDangerous)) {
            // Order 2: DG Class - Mandatory when DG is On (using hazardLabel field)
            if (!StringUtils.hasText(request.getHazardLabel())) {
                TICommonValidator.hazardLabelValidation(context);
                isValid = false;
            }

            // Order 3: UN Number - Mandatory when DG is On
            if (!StringUtils.hasText(request.getUnNumber())) {
                TICommonValidator.unNumberValidation(context);
                isValid = false;
            }

            // Order 4: DG Class Description - Mandatory when DG is On
            if (!StringUtils.hasText(request.getDgClassDescription())) {
                TICommonValidator.dgClassDescriptionValidation(context);
                isValid = false;
            }
        }

        return isValid;
    }

    // You'll need to implement this method based on your business logic
    private String getTransportMode(TransportInstructionLegsPackagesRequest request) {
        return request.getTransportMode();
    }
}
