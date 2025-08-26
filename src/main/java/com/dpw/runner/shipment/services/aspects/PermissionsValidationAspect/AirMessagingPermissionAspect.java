package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.Generated;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.*;


@Aspect
@Component @Generated
public class AirMessagingPermissionAspect {

    @Value("${awb.air-message.status.reset.permission}")
    private String AIR_MESSAGE_STATUS_RESET_PERMISSION;

    @Before("execution(* com.dpw.runner.shipment.services.service.impl.AwbService.airMessageStatusReset(..)) && args(commonRequestModel)")
    public void airMessageStatusReset(JoinPoint joinPoint, CommonRequestModel commonRequestModel) throws RunnerException {
        if (Objects.equals(AIR_MESSAGE_STATUS_RESET_PERMISSION, "IGNORE")) {
            return;
        }

        if (!UserContext.getUser().getPermissions().containsKey(AIR_MESSAGE_STATUS_RESET_PERMISSION)) {
            throw new ValidationException("Insufficient permissions to reset air messaging status");
        }
    }

}
