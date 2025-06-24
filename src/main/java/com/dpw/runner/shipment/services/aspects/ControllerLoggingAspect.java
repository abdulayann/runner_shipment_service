package com.dpw.runner.shipment.services.aspects;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

@Aspect
@Component
@Slf4j
@Generated
public class ControllerLoggingAspect {

    @Autowired
    private JsonHelper jsonHelper;

    // Pointcut to intercept any method within a class annotated with @RestController or @Controller
    @Pointcut("within(@org.springframework.web.bind.annotation.RestController *) || within(@org.springframework.stereotype.Controller *)")
    public void controllerMethods() {}

    @Around("controllerMethods()")
    public Object logSingleLineRequest(ProceedingJoinPoint joinPoint) throws Throwable {
        HttpServletRequest request =
                ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();

        String method = request.getMethod();
        String url = request.getRequestURI();
        String query = request.getQueryString();

        // Collect headers
        Map<String, String> headers = new HashMap<>();
        Enumeration<String> headerNames = request.getHeaderNames();
        while (headerNames.hasMoreElements()) {
            String name = headerNames.nextElement();
            String value = name.equalsIgnoreCase("authorization") ? "*****" : request.getHeader(name);
            headers.put(name, value);
        }

        List<String> jsonBodies = new ArrayList<>();
        for (Object arg : joinPoint.getArgs()) {
            if (arg != null && !(arg instanceof HttpServletRequest)) {
                try {
                    String json = jsonHelper.convertToJson(arg);
                    jsonBodies.add(json);
                } catch (Exception e) {
                    jsonBodies.add("\"[Unserializable body: " + e.getMessage() + "]\"");
                }
            }
        }


        log.info("{} | REQUEST RECEIVED [HTTP Method={}] [URL={}] [QUERY={}] [HEADERS={}] [BODY={}]",
                LoggerHelper.getRequestIdFromMDC(),
                method,
                url,
                query != null ? query : "",
                headers,
                jsonBodies);

        Object response = null;
        try {
            response = joinPoint.proceed();
            return response;
        } finally {
            String responseLog;
            try {
                responseLog = jsonHelper.convertToJson(response);
            } catch (Exception e) {
                responseLog = "[Unserializable Response: " + e.getMessage() + "]";
            }
            log.info("{} | RESPONSE RETURNED [RESPONSE={}]",
                    LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(responseLog));
        }
    }
}
