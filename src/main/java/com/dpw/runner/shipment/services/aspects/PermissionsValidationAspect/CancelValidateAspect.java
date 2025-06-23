package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;

@Aspect
@Component
public class CancelValidateAspect {

    private final IShipmentDao shipmentDao;

    @Autowired
    public CancelValidateAspect(IShipmentDao shipmentDao) {
        this.shipmentDao = shipmentDao;
    }


    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.cancel(..)) && args(commonRequestModel)")
    public void validateShipmentCancel(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_CANCEL_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        ShipmentDetails shipmentRequest = getShipment(commonRequestModel);
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

            if (getValidateFields(mappedPermissionList, transportMode, validatedFields, direction, shipmentType, domesticType, retrieveValidationFields))
                return;

            if (validatedFields.size() < retrieveValidationFields)
                throw new RunnerException("Unavailable to cancel Shipment due to insufficient cancel permissions");
        }
    }

    private boolean getValidateFields(List<String> mappedPermissionList, String transportMode, Set<String> validatedFields, String direction, String shipmentType, Boolean domesticType, int retrieveValidationFields) {
        for (String v1MappedPermission : mappedPermissionList){
            if(v1MappedPermission == null)
                continue;
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("cancel"))
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


    @Before("execution(* com.dpw.runner.shipment.services.service.impl.ShipmentService.cancel(..)) && args(commonRequestModel)")
    public void validateConsolidationCancel(JoinPoint joinPoint, CommonRequestModel commonRequestModel) {
        // empty since we don't have cancel status in consolidation yet !
    }

    private ShipmentDetails getShipment(CommonRequestModel commonRequestModel) {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> optional = shipmentDao.findById(commonGetRequest.getId());
        return optional.orElse(null);
    }

}
