package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;

@Aspect
@Component
public class UpdateValidateAspect {

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.completeUpdate(..)) && args(commonRequestModel)")
    public void validateShipmentUpdateV2(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentRequest shipment = (ShipmentRequest) commonRequestModel.getData();
        if (Objects.nonNull(shipment)) {
            this.validateShipmentUpdate(shipment.getTransportMode(),
                    shipment.getDirection(),
                    shipment.getShipmentType(),
                    shipment.getIsDomestic());
        }
    }

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentServiceImplV3.completeUpdate(..)) && args(commonRequestModel)")
    public void validateShipmentUpdateV3(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentV3Request shipment = (ShipmentV3Request) commonRequestModel.getData();
        if (Objects.nonNull(shipment)) {
            this.validateShipmentUpdate(shipment.getTransportMode(),
                    shipment.getDirection(),
                    shipment.getShipmentType(),
                    shipment.getIsDomestic());
        }
    }

    public void validateShipmentUpdate(String transportMode,
                                String direction,
                                String shipmentType,
                                Boolean isDomestic) throws RunnerException {

        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_UPDATE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        Boolean domesticType = null;

        if (Objects.nonNull(transportMode)) transportMode = transportMode.toLowerCase();
        if (Objects.nonNull(direction)) direction = direction.toLowerCase();
        if (Objects.nonNull(shipmentType)) shipmentType = shipmentType.toLowerCase();
        if (Objects.nonNull(isDomestic)) domesticType = isDomestic;

        List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

        if (getValidatedFields(mappedPermissionList, transportMode, validatedFields, direction, shipmentType, domesticType, retrieveValidationFields))
            return;

        if (validatedFields.size() < retrieveValidationFields)
            throw new RunnerException("Unavailable to update record due to insufficient update permissions");
    }


    private boolean getValidatedFields(List<String> mappedPermissionList, String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, int retrieveValidationFields) {
        for (String v1MappedPermission : mappedPermissionList){
            // earlier used String v1MappedPermission = V1PermissionMapUtil.getPermissionName(permission)
            if(v1MappedPermission == null)
                continue;
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("update"))
                    .toList();
            setValidatedFields(transportMode, validatedFields, direction, shipmentType, domesticType, parameterList);
            if(validatedFields.size() == retrieveValidationFields)
                return true;
        }
        return false;
    }

    private void setValidatedFields(String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, List<String> parameterList) {
        String validTransportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
        String validDirection = getParameterFromPermission(DIRECTION_INDEX, parameterList);
        String validShipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
        String validDomesticType = getParameterFromPermission(IS_DOMESTIC_INDEX, parameterList);

        if(validTransportMode.equals(ALL) || transportMode == null || transportMode.equals(validTransportMode)){
            validatedFields.add("transportMode");
            if(validDirection.equals(ALL) || direction == null || direction.equals(validDirection)){
                validatedFields.add("direction");
                if(validShipmentType.equals(ALL) || shipmentType == null || shipmentType.equals(validShipmentType)){
                    validatedFields.add("shipmentType");
                    if(validDomesticType.equals(ALL) || domesticType == null || domesticType.equals(validDomesticType.equals(DOMESTIC))){
                        validatedFields.add("domesticType");
                    }
                }
            }
        }
    }

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ConsolidationService.completeUpdate(..)) && args(commonRequestModel)")
    public void validateConsolidationUpdateV2(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        this.validateConsolidationUpdate(joinPoint, commonRequestModel);
    }

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ConsolidationV3Service.completeUpdate(..)) && args(commonRequestModel)")
    public void validateConsolidationUpdateV3(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        this.validateConsolidationUpdate(joinPoint, commonRequestModel);
    }

    public void validateConsolidationUpdate(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        List<String> userPermissions = PermissionsContext.getPermissions(CONSOLIDATION_UPDATE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        ConsolidationDetailsRequest consolidation = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if(consolidation != null){
            String transportMode = null;
            String direction = null;
            String shipmentType = null;
            Boolean domesticType = null;
            if(consolidation.getTransportMode() != null)
                transportMode = consolidation.getTransportMode().toLowerCase();
            if(consolidation.getShipmentType() != null)
                direction = consolidation.getShipmentType().toLowerCase();
            if(consolidation.getContainerCategory() != null)
                shipmentType = consolidation.getContainerCategory().toLowerCase();
            if(consolidation.getIsDomestic() != null)
                domesticType = consolidation.getIsDomestic();

            List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

            if (getValidatedFields(mappedPermissionList, transportMode, validatedFields, direction, shipmentType, domesticType, retrieveValidationFields))
                return;

            if (validatedFields.size() < retrieveValidationFields)
                throw new RunnerException("Unavailable to update record due to insufficient update permissions");
        }
    }
}
