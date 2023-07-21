package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Slf4j
@Component
public class LoggerHelper {
    private static Boolean printPayload;
    @Value("${logging.print-payload}")
    public void setPrintPayload(Boolean printPayload){
        LoggerHelper.printPayload = printPayload;
    }
    public static String getRequestIdFromMDC(){
        return MDC.get(LoggingConstants.REQUEST_ID);
    }

    public static void putRequestId(String requestId) {
        if(requestId == null || requestId.isEmpty()) {
            requestId = UUID.randomUUID().toString();
        }
        MDC.put(LoggingConstants.REQUEST_ID, requestId);
    }

    public static void debug(String msg, String payload){
        if (printPayload) {
            log.debug(msg + " with Request Id {} and payload {}", getRequestIdFromMDC(), payload);
        } else {
            log.debug(msg + "with Request Id {}", getRequestIdFromMDC());
        }
    }
    public static void debug(String msg){
            log.debug(msg + "with Request Id {}", getRequestIdFromMDC());
    }

    public static void info(String msg, String payload){
        if(printPayload)
            log.info(msg+" with Request Id {} and payload {}", getRequestIdFromMDC(), payload);
        else
            log.info(msg + "with Request Id {}", getRequestIdFromMDC());
    }
    public static void info(String msg){
        log.info(msg+" with Request Id {}", getRequestIdFromMDC());
    }

    public static void warn(String msg, String payload){
        if(printPayload)
            log.warn(msg+" with Request Id {} and payload {}", getRequestIdFromMDC(), payload);
        else
            log.warn(msg+" with Request Id {}", getRequestIdFromMDC());
    }
    public static void warn(String msg){
        log.warn(msg+" with Request Id {}", getRequestIdFromMDC());
    }

    public static void error(String msg, String payload){
        if(printPayload)
            log.error(msg+" with Request Id {} and payload {}", getRequestIdFromMDC(), payload);
        else
            log.error(msg+" with Request Id {}", getRequestIdFromMDC());
    }
    public static void error(String msg){
        log.error(msg+" with Request Id {}", getRequestIdFromMDC());
    }

    public static void trace(String msg, String payload){
        if(printPayload)
            log.trace(msg+" with Request Id {} and payload {}", getRequestIdFromMDC(), payload);
        else
            log.trace(msg+" with Request Id {}", getRequestIdFromMDC());
    }
    public static void trace(String msg){
        log.trace(msg+" with Request Id {}", getRequestIdFromMDC());
    }

}
