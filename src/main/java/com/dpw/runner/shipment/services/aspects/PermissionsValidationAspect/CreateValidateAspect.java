package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;
import static com.dpw.runner.shipment.services.utils.V1PermissionMapUtil.getPermissionName;

@Aspect
@Component
public class CreateValidateAspect {

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.create(..)) && args(commonRequestModel)")
    public void validateShipmentCreate(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentRequest shipment = (ShipmentRequest) commonRequestModel.getData();
        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_CREATE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        if(shipment != null){
            String transportMode = null, direction = null, shipmentType = null;
            Boolean domesticType = null;
            if(shipment.getTransportMode() != null)
                transportMode = shipment.getTransportMode().toLowerCase();
            if(shipment.getDirection() != null)
                direction = shipment.getDirection().toLowerCase();
            if(shipment.getShipmentType() != null)
                shipmentType = shipment.getShipmentType().toLowerCase();
            if(shipment.getIsDomestic() != null)
                domesticType = shipment.getIsDomestic();

            for (String permission : userPermissions){
                String v1MappedPermission = V1PermissionMapUtil.getPermissionName(permission);
                if(v1MappedPermission == null)
                    continue;
                List<String> parameterList = Arrays.stream(getPermissionName(permission).toLowerCase().split(DELIMITER))
                        .filter(e -> !e.contains("create"))
                        .toList();
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
                if(validatedFields.size() == retrieveValidationFields)
                    return;
            }

            if (validatedFields.size() < retrieveValidationFields)
                throw new RunnerException("Unable to create record due to insufficient create permissions");
        }
    }

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ConsolidationService.create(..)) && args(commonRequestModel)")
    public void validateConsolidationCreate(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        List<String> userPermissions = PermissionsContext.getPermissions(CONSOLIDATION_CREATE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        ConsolidationDetailsRequest consolidation = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if(consolidation != null){
            String transportMode = null, direction = null, shipmentType = null;
            Boolean domesticType = null;
            if(consolidation.getTransportMode() != null)
                transportMode = consolidation.getTransportMode().toLowerCase();
            if(consolidation.getShipmentType() != null)
                direction = consolidation.getShipmentType().toLowerCase();
            if(consolidation.getContainerCategory() != null)
                shipmentType = consolidation.getContainerCategory().toLowerCase();
            if(consolidation.getIsDomestic() != null)
                domesticType = consolidation.getIsDomestic();

            for (String permission : userPermissions){
                String v1MappedPermission = V1PermissionMapUtil.getPermissionName(permission);
                if(v1MappedPermission == null)
                    continue;
                List<String> parameterList = Arrays.stream(getPermissionName(permission).toLowerCase().split(DELIMITER))
                        .filter(e -> !e.contains("create"))
                        .toList();
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
                if(validatedFields.size() == retrieveValidationFields)
                    return;
            }

            if (validatedFields.size() < retrieveValidationFields)
                throw new RunnerException("Unable to create record due to insufficient create permissions");
        }
    }
}
