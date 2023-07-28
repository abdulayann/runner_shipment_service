package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.V1PermissionMapUtil;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.PermissionUtil.getParameterFromPermission;
import static com.dpw.runner.shipment.services.utils.V1PermissionMapUtil.getPermissionName;

@Aspect
@Component
@Slf4j
public class RetrieveValidateAspect {

    @AfterReturning(pointcut="execution(* com.dpw.runner.shipment.services.dao.impl.ShipmentDao.findById(..)) && args(Long)",
            returning="shipment")
    public void validateShipmentRetrieve(Optional<ShipmentDetails> shipment) throws RunnerException {
        log.info("Validating Retrieve permissions on shipment entity");
        List<String> userPermissions = PermissionsContext.getPermissions(SHIPMENT_RETRIEVE_PERMISSION);
        int retrieveValidationFields = 4;
        Set<String> validatedFields = new HashSet<>();
        if(shipment.isPresent()){
            String transportMode = shipment.get().getTransportMode().toLowerCase();
            String direction = shipment.get().getDirection().toLowerCase();
            String shipmentType = shipment.get().getShipmentType().toLowerCase();
            Boolean domesticType = shipment.get().getIsDomestic();

            for (String permission : userPermissions){
                String v1MappedPermission = V1PermissionMapUtil.getPermissionName(permission);
                if(v1MappedPermission == null)
                    continue;
                List<String> parameterList = Arrays.stream(getPermissionName(permission).toLowerCase().split(DELIMITER))
                        .filter(e -> !e.contains("retrieve"))
                        .collect(Collectors.toList());
                String validTransportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
                String validDirection = getParameterFromPermission(DIRECTION_INDEX, parameterList);
                String validShipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
                String validDomesticType = getParameterFromPermission(IS_DOMESTIC_INDEX, parameterList);

                if(validTransportMode.equals(ALL) || transportMode.equals(validTransportMode))
                    validatedFields.add(transportMode);
                if(validDirection.equals(ALL) || direction.equals(validDirection))
                    validatedFields.add(direction);
                if(validShipmentType.equals(ALL) || shipmentType.equals(validShipmentType))
                    validatedFields.add(shipmentType);
                if(validDomesticType.equals(ALL) || domesticType.equals(validDomesticType.equals(DOMESTIC)))
                    validatedFields.add(domesticType.toString());

                if(validatedFields.size() == retrieveValidationFields)
                    return;
            }

            if (validatedFields.size() < retrieveValidationFields)
                throw new RunnerException("Unavailable to retrieve record due to insufficient retrieve permissions");
        }
    }
}
