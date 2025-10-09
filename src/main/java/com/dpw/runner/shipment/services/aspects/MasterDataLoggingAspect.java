package com.dpw.runner.shipment.services.aspects;

import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.JoinPoint;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import static com.dpw.runner.shipment.services.commons.constants.Constants.ORIGINATED_FROM;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_SERVICE_TYPE;

@Component @Aspect @Generated @Slf4j
public class MasterDataLoggingAspect {

    @Before("execution(* com.dpw.runner.shipment.services.controller.MasterDataController.*(..))")
    public void logSourceServiceType(JoinPoint joinPoint) {
        try {
            ServletRequestAttributes attrs = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
            if (attrs != null) {
                HttpServletRequest request = attrs.getRequest();
                String originatedFrom = request.getHeader(SOURCE_SERVICE_TYPE);
                if (StringUtility.isNotEmpty(originatedFrom))
                    MDC.put(ORIGINATED_FROM, originatedFrom);
            }
        } catch (Exception e) {
            log.error("logSourceServiceType: {}", e.getMessage());
        }
    }
}
