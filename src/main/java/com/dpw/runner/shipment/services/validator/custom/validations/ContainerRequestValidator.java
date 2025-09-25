package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import com.dpw.runner.shipment.services.validator.annotations.ValidContainerRequest;
import org.springframework.util.StringUtils;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class ContainerRequestValidator implements ConstraintValidator<ValidContainerRequest, TransportInstructionLegsContainersRequest> {

    @Override
    public boolean isValid(TransportInstructionLegsContainersRequest request, ConstraintValidatorContext context) {
        if (request == null) {
            return true; // Let @NotNull handle null validation
        }

        boolean isValid = true;
        context.disableDefaultConstraintViolation();

        // Determine mode from context or pass as parameter
        String mode = getTransportMode(request); // You'll need to implement this method
        Boolean isDangerous = request.getDangerous();

        if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(mode)) {
            isValid = validateSeaContainerRequest(request, context, isDangerous);
        } else if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(mode)) {
            isValid = validateAirContainerRequest(request, context, isDangerous);
        }

        return isValid;
    }

    private boolean validateSeaContainerRequest(TransportInstructionLegsContainersRequest request,
                                                ConstraintValidatorContext context, Boolean isDangerous) {
        boolean isValid = true;

        if (Boolean.TRUE.equals(isDangerous)) {
            // Order 2: DG Class - Mandatory when DG is On
            if (!StringUtils.hasText(request.getDgClass())) {
                TICommonValidator.dgClassValidation(context);
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

    private boolean validateAirContainerRequest(TransportInstructionLegsContainersRequest request,
                                                ConstraintValidatorContext context, Boolean isDangerous) {
        boolean isValid = true;

        if (Boolean.TRUE.equals(isDangerous)) {
            // Order 2: DG Class - Mandatory when DG is On
            if (!StringUtils.hasText(request.getDgClass())) {
                TICommonValidator.dgClassValidation(context);
                isValid = false;
            }

            // Order 3: UN Number - Mandatory when DG is On
            if (!StringUtils.hasText(request.getUnNumber())) {
                TICommonValidator.unNumberValidation(context);
                isValid = false;
            }

            // Note: DG Class Description is not present in container request
            // This validation would be needed if the field is added
        }

        return isValid;
    }

    // You'll need to implement this method based on your business logic
    private String getTransportMode(TransportInstructionLegsContainersRequest request) {
        return request.getTransportMode();
    }
}
