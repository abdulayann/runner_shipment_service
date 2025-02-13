package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;

@Aspect
@Component
@Slf4j
public class RetrieveValidateAspect {

    @AfterReturning(pointcut = "execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.retireveShipmentData(..))",
            returning = "shipmentResponse")
    public void validateShipmentRetrieve(ShipmentDetailsResponse shipmentResponse) throws ValidationException {
        log.info("Validating Retrieve permissions on shipment entity");
        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_RETRIEVE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        String transportMode = null, direction = null, shipmentType = null;
        Boolean domesticType = null;
        if (shipmentResponse.getTransportMode() != null)
            transportMode = shipmentResponse.getTransportMode().toLowerCase();
        if (shipmentResponse.getDirection() != null)
            direction = shipmentResponse.getDirection().toLowerCase();
        if (shipmentResponse.getShipmentType() != null)
            shipmentType = shipmentResponse.getShipmentType().toLowerCase();
        if (shipmentResponse.getIsDomestic() != null)
            domesticType = shipmentResponse.getIsDomestic();

        List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

        for (String v1MappedPermission : mappedPermissionList) {
            if (v1MappedPermission == null)
                continue;
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("retrieve"))
                    .toList();
            String validTransportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
            String validDirection = getParameterFromPermission(DIRECTION_INDEX, parameterList);
            String validShipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
            String validDomesticType = getParameterFromPermission(IS_DOMESTIC_INDEX, parameterList);

            if (validTransportMode.equals(ALL) || transportMode == null || transportMode.equals(validTransportMode)) {
                validatedFields.add("transportMode");
                if (validDirection.equals(ALL) || direction == null || direction.equals(validDirection)) {
                    validatedFields.add("direction");
                    if (validShipmentType.equals(ALL) || shipmentType == null || shipmentType.equals(validShipmentType)) {
                        validatedFields.add("shipmentType");
                        if (validDomesticType.equals(ALL) || domesticType == null || domesticType.equals(validDomesticType.equals(DOMESTIC))) {
                            validatedFields.add("domesticType");
                        }
                    }
                }
            }
            if (validatedFields.size() == retrieveValidationFields)
                return;
        }

        if (validatedFields.size() < retrieveValidationFields) {
            throw new ValidationException("Unavailable to retrieve record due to insufficient retrieve permissions");
        }

    }

    @AfterReturning(pointcut = "execution(* com.dpw.runner.shipment.services.service.impl.ConsolidationService.retrieveConsolidationData(..))",
            returning = "consolidationResponse")
    public void validateConsolidationRetrieve(ConsolidationDetailsResponse consolidationResponse) throws ValidationException {
        log.info("Validating Retrieve permissions on consolidation entity");
        List<String> userPermissions = PermissionsContext.getPermissions(CONSOLIDATION_RETRIEVE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        String transportMode = null, direction = null, shipmentType = null;
        Boolean domesticType = null;
        if (consolidationResponse.getTransportMode() != null)
            transportMode = consolidationResponse.getTransportMode().toLowerCase();
        if (consolidationResponse.getShipmentType() != null)
            direction = consolidationResponse.getShipmentType().toLowerCase();
        if (consolidationResponse.getContainerCategory() != null)
            shipmentType = consolidationResponse.getContainerCategory().toLowerCase();
        if (consolidationResponse.getIsDomestic() != null)
            domesticType = consolidationResponse.getIsDomestic();

        List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

        for (String v1MappedPermission : mappedPermissionList) {
            if (v1MappedPermission == null)
                continue;
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("retrieve"))
                    .toList();
            String validTransportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
            String validDirection = getParameterFromPermission(DIRECTION_INDEX, parameterList);
            String validShipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
            String validDomesticType = getParameterFromPermission(IS_DOMESTIC_INDEX, parameterList);

            if (validTransportMode.equals(ALL) || transportMode == null || transportMode.equals(validTransportMode)) {
                validatedFields.add("transportMode");
                if (validDirection.equals(ALL) || direction == null || direction.equals(validDirection)) {
                    validatedFields.add("direction");
                    if (validShipmentType.equals(ALL) || shipmentType == null || shipmentType.equals(validShipmentType)) {
                        validatedFields.add("shipmentType");
                        if (validDomesticType.equals(ALL) || domesticType == null || domesticType.equals(validDomesticType.equals(DOMESTIC))) {
                            validatedFields.add("domesticType");
                        }
                    }
                }
            }
            if (validatedFields.size() == retrieveValidationFields)
                return;
        }

        if (validatedFields.size() < retrieveValidationFields)
            throw new ValidationException("Unavailable to retrieve record due to insufficient retrieve permissions");
    }
}
