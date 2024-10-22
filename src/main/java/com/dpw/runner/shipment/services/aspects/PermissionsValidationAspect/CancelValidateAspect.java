package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;

@Aspect
public class CancelValidateAspect {


    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.cancel(..)) && args(commonRequestModel)")
    public void validateShipmentCancel(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_CANCEL_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();
        if(shipmentRequest != null){
            String transportMode = null;
            String direction = null;
            String shipmentType = null;
            Boolean domesticType = null;
            if(shipmentRequest.getTransportMode() != null)
                transportMode = shipmentRequest.getTransportMode().toLowerCase();
            if(shipmentRequest.getDirection() != null)
                direction = shipmentRequest.getDirection().toLowerCase();
            if(shipmentRequest.getShipmentType() != null)
                shipmentType = shipmentRequest.getShipmentType().toLowerCase();
            if(shipmentRequest.getIsDomestic() != null)
                domesticType = shipmentRequest.getIsDomestic();

            List<String> mappedPermissionList = V1PermissionMapUtil.getPermissionNames(userPermissions);

            for (String v1MappedPermission : mappedPermissionList){
                if(v1MappedPermission == null)
                    continue;
                List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                        .filter(e -> !e.contains("cancel"))
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
                throw new RunnerException("Unavailable to cancel Shipment due to insufficient update permissions");
        }
    }



    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.cancel(..)) && args(commonRequestModel)")
    public void validateConsolidationCancel(JoinPoint joinPoint, CommonRequestModel commonRequestModel) {
        // empty since we don't have cancel status in consolidation yet !
    }

}
