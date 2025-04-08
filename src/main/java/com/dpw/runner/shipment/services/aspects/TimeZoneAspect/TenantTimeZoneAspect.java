package com.dpw.runner.shipment.services.aspects.TimeZoneAspect;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.TimeZoneConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.MDC;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;


@Component
@Aspect
@Slf4j
public class TenantTimeZoneAspect {

    @Pointcut("execution(* com.dpw.runner.shipment.services.controller.*.*(..))" + "&& !@annotation(com.dpw.runner.shipment.services.utils.ExcludeTimeZone)")
    public void controllerMethods() {
        /**
         * Method left empty
         */
    }

    @Around("controllerMethods()")
    public Object changeTimeZone(ProceedingJoinPoint joinPoint) throws Throwable {
        log.info("Inside TenantTimeZone Aspect");

        Object[] args = joinPoint.getArgs();

        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = userDetails.getEnableTimeZone();
        String tenantTimeZone = userDetails.getTimeZoneId();
        String browserTimeZone = fetchTimeZoneIdFromRequestHeader();
        boolean skipTimeZone = skipTimeZone();

        //for handling request
        for (Object arg : args) {
            if(arg != null && !skipTimeZone)
                transformTimeZoneRecursively(arg, browserTimeZone, tenantTimeZone, enableTimeZoneFlag, true);
        }

        //Sending request and catching response
        Object response = joinPoint.proceed(args);

        // For handling Response
        if (response instanceof ResponseEntity<?>) {
            Object body = ((ResponseEntity<?>) response).getBody();
            if (body != null && !skipTimeZone) {
                transformTimeZoneRecursively(body, browserTimeZone, tenantTimeZone, enableTimeZoneFlag, false);
            }
        }

        return response;
    }

    /**
     * Recursive Function to transform Date Fields for Nested Fields
     * @param arg - Request Object
     * @param browserTimeZone - Browser Time Zone From Request
     * @param tenantTimeZone - Tenant Time Zone (Consider only when enableTimeZoneFlag is true)
     * @param enableTimeZoneFlag - Whether to consider Tenant Time Zone or Not
     * @param beforeExecution - To check whether timeZone Changes should be for Request(true) Or for Response(false)
     */
    @SuppressWarnings("java:S135")
    private void transformTimeZoneRecursively(Object arg, String browserTimeZone, String tenantTimeZone, boolean enableTimeZoneFlag, boolean beforeExecution) throws IllegalAccessException {
        Class<?> currentFieldClass = arg.getClass();

        while (currentFieldClass != null) {
            for (Field field : currentFieldClass.getDeclaredFields()) {
                if (field.isAnnotationPresent(ExcludeTimeZone.class)) continue;

                if(!field.trySetAccessible()) continue;

                field.setAccessible(true);
                Object fieldValue = field.get(arg);

                if (fieldValue != null) {
                    processFieldValue(arg, browserTimeZone, tenantTimeZone, enableTimeZoneFlag, beforeExecution, field, fieldValue);
                }
            }
            currentFieldClass = currentFieldClass.getSuperclass();
        }

    }

    private void processFieldValue(Object arg, String browserTimeZone, String tenantTimeZone, boolean enableTimeZoneFlag, boolean beforeExecution, Field field, Object fieldValue) throws IllegalAccessException {
        if (fieldValue instanceof List<?> list) {
            for (Object listItem : list) {
                transformTimeZoneRecursively(listItem, browserTimeZone, tenantTimeZone, enableTimeZoneFlag, beforeExecution);
            }

        } else if (fieldValue instanceof LocalDateTime inputDate) {
            setTransformedDate(arg, browserTimeZone, tenantTimeZone, enableTimeZoneFlag, beforeExecution, field, inputDate);

        } else if (!(fieldValue instanceof Enum<?>)
                && fieldValue.getClass().getName().startsWith("com.dpw")
        ) {
            transformTimeZoneRecursively(fieldValue, browserTimeZone, tenantTimeZone, enableTimeZoneFlag, beforeExecution);
        }
    }

    private void setTransformedDate(Object arg, String browserTimeZone, String tenantTimeZone, boolean enableTimeZoneFlag, boolean beforeExecution, Field field, LocalDateTime inputDate) throws IllegalAccessException {
        LocalDateTime transformedDate;
        if (beforeExecution) {
            transformedDate = DateUtils.convertDateFromUserTimeZone(
                    inputDate, browserTimeZone, tenantTimeZone, enableTimeZoneFlag);
        } else {
            transformedDate = DateUtils.convertDateToUserTimeZone(
                    inputDate, browserTimeZone, tenantTimeZone, enableTimeZoneFlag);
        }
        field.set(arg, transformedDate);
    }


    private String fetchTimeZoneIdFromRequestHeader() {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        String xBrowserTimeZone = TimeZoneConstants.DEFAULT_TIME_ZONE_ID;
        if (Objects.nonNull(requestAttributes)) {
            ServletRequestAttributes attributes = (ServletRequestAttributes) requestAttributes;
            xBrowserTimeZone = attributes.getRequest().getHeader(TimeZoneConstants.BROWSER_TIME_ZONE_NAME);
            if (StringUtils.isNotBlank(xBrowserTimeZone)) {
                xBrowserTimeZone = xBrowserTimeZone.replaceAll("\\s", "").trim().strip();
            }
        }
        MDC.put(TimeZoneConstants.BROWSER_TIME_ZONE_NAME, xBrowserTimeZone);
        return xBrowserTimeZone;
    }

    private boolean skipTimeZone() {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        if(Objects.nonNull(requestAttributes))
        {
            ServletRequestAttributes attributes = (ServletRequestAttributes) requestAttributes;
            String skipTimeZone = attributes.getRequest().getHeader(TimeZoneConstants.SKIP_TIME_ZONE);
            return Objects.equals(skipTimeZone, "true");
        }
        return false;
    }
}
