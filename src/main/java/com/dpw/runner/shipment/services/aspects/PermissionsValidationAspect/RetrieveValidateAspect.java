package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;

@Aspect
@Component
@Slf4j
public class RetrieveValidateAspect {

    @AfterReturning(pointcut = "execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.retireveShipmentData(..))",
            returning = "shipmentResponse")
    public void validateShipmentRetrieveV2(ShipmentDetailsResponse shipmentResponse) throws ValidationException {
        this.validateShipmentRetrieve(
                StringUtility.toLowerCase(shipmentResponse.getTransportMode()),
                StringUtility.toLowerCase(shipmentResponse.getDirection()),
                StringUtility.toLowerCase(shipmentResponse.getShipmentType()),
                shipmentResponse.getIsDomestic()
        );
    }

    @AfterReturning(pointcut = "execution(* com.dpw.runner.shipment.services.service.impl.ShipmentServiceImplV3.retireveShipmentData(..))",
            returning = "shipmentResponse")
    public void validateShipmentRetrieveV3(ShipmentRetrieveLiteResponse shipmentResponse) throws ValidationException {
        this.validateShipmentRetrieve(
                StringUtility.toLowerCase(shipmentResponse.getTransportMode()),
                StringUtility.toLowerCase(shipmentResponse.getDirection()),
                StringUtility.toLowerCase(shipmentResponse.getShipmentType()),
                shipmentResponse.getIsDomestic()
        );
    }

    public void validateShipmentRetrieve(String transportMode, String direction, String shipmentType, Boolean domesticType) throws ValidationException {
        log.info("Validating Retrieve permissions on shipment entity");
        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_RETRIEVE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();


        List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

        if (getShipmentRetrieveValidatedFields(mappedPermissionList, transportMode, validatedFields, direction, shipmentType, domesticType, retrieveValidationFields))
            return;

        if (validatedFields.size() < retrieveValidationFields) {
            throw new ValidationException("Unavailable to retrieve record due to insufficient retrieve permissions");
        }

    }

    private boolean getShipmentRetrieveValidatedFields(List<String> mappedPermissionList, String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, int retrieveValidationFields) {
        for (String v1MappedPermission : mappedPermissionList) {
            if (v1MappedPermission == null)
                continue;
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("retrieve"))
                    .toList();
            setShipmentValidatedFields(transportMode, validatedFields, direction, shipmentType, domesticType, parameterList);
            if (validatedFields.size() == retrieveValidationFields)
                return true;
        }
        return false;
    }

    private void setShipmentValidatedFields(String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, List<String> parameterList) {
        setValidatedFields(transportMode, validatedFields, direction, shipmentType, domesticType, parameterList);
    }

    @AfterReturning(pointcut = "execution(* com.dpw.runner.shipment.services.service.impl.ConsolidationService.retrieveConsolidationData(..))",
            returning = "consolidationResponse")
    public void validateConsolidationRetrieveForV2(ConsolidationDetailsResponse consolidationResponse) throws ValidationException {
        this.validateConsolidationRetrieve(
                StringUtility.toLowerCase(consolidationResponse.getTransportMode()),
                StringUtility.toLowerCase(consolidationResponse.getShipmentType()),
                StringUtility.toLowerCase(consolidationResponse.getContainerCategory()),
                consolidationResponse.getIsDomestic()
        );
    }


    @AfterReturning(pointcut = "execution(* com.dpw.runner.shipment.services.service.impl.ConsolidationV3Service.retrieveById(..))",
            returning = "consolidationResponse")
    public void validateConsolidationRetrieveV3(ConsolidationDetailsV3Response consolidationResponse) throws ValidationException {
        this.validateConsolidationRetrieve(
                StringUtility.toLowerCase(consolidationResponse.getTransportMode()),
                StringUtility.toLowerCase(consolidationResponse.getShipmentType()),
                StringUtility.toLowerCase(consolidationResponse.getContainerCategory()),
                consolidationResponse.getIsDomestic()
        );
    }
    public void validateConsolidationRetrieve(String transportMode, String direction, String shipmentType, Boolean domesticType) throws ValidationException {
        log.info("Validating Retrieve permissions on consolidation entity");
        List<String> userPermissions = PermissionsContext.getPermissions(CONSOLIDATION_RETRIEVE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();

        List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

        if (getValidatedFields(mappedPermissionList, transportMode, validatedFields, direction, shipmentType, domesticType, retrieveValidationFields))
            return;

        if (validatedFields.size() < retrieveValidationFields)
            throw new ValidationException("Unavailable to retrieve record due to insufficient retrieve permissions");
    }

    private boolean getValidatedFields(List<String> mappedPermissionList, String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, int retrieveValidationFields) {
        for (String v1MappedPermission : mappedPermissionList) {
            if (v1MappedPermission == null)
                continue;
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("retrieve"))
                    .toList();
            setValidatedFields(transportMode, validatedFields, direction, shipmentType, domesticType, parameterList);
            if (validatedFields.size() == retrieveValidationFields)
                return true;
        }
        return false;
    }

    private void setValidatedFields(String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, List<String> parameterList) {
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
    }
}
