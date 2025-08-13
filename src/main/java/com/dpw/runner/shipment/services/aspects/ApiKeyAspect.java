package com.dpw.runner.shipment.services.aspects;

import com.dpw.runner.shipment.services.authservice.ApiKeyService;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;

@Aspect
@Component
public class ApiKeyAspect {
    
    @Autowired
    private ApiKeyService apiKeyService;
    
    @Around("@annotation(com.dpw.runner.shipment.services.annotations.RequireApiKey)")
    public Object validateApiKey(ProceedingJoinPoint joinPoint) throws Throwable {
        HttpServletRequest request = getCurrentRequest();
        
        if (request == null) {
            throw new UnAuthorizedException("Cannot access request");
        }
        
        String apiKey = request.getHeader("x-api-key");
        
        if (apiKey == null || !apiKeyService.isValidApiKey(apiKey)) {
            throw new UnAuthorizedException("Valid API key required");
        }
        
        return joinPoint.proceed();
    }
    
    private HttpServletRequest getCurrentRequest() {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        if (requestAttributes instanceof ServletRequestAttributes) {
            return ((ServletRequestAttributes) requestAttributes).getRequest();
        }
        return null;
    }
}