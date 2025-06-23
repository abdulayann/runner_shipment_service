package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.UUID;

import static com.dpw.runner.shipment.services.commons.constants.Constants.WITH_REQUEST_ID_MSG;

@Slf4j
@Component
@Generated
public class LoggerHelper {
    private LoggerHelper(){}
    public static final String WITH_REQUEST_ID_AND_PAYLOAD_MSG = " with Request Id {} and payload {}";

    private static Boolean printPayload;
    @Value("${logging.print-payload}")
    public static void setPrintPayload(Boolean printPayload){
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
        if (Boolean.TRUE.equals(printPayload)) {
            log.debug(msg + WITH_REQUEST_ID_AND_PAYLOAD_MSG, getRequestIdFromMDC(), payload);
        } else {
            log.debug(msg + WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
        }
    }
    public static void debug(String msg){
            log.debug(msg + WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }

    public static void info(String msg, String payload){
        if(Boolean.TRUE.equals(printPayload))
            log.info(msg+ WITH_REQUEST_ID_AND_PAYLOAD_MSG, getRequestIdFromMDC(), payload);
        else
            log.info(msg + WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }
    public static void info(String msg){
        log.info(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }

    public static void warn(String msg, String payload){
        if(Boolean.TRUE.equals(printPayload))
            log.warn(msg+ WITH_REQUEST_ID_AND_PAYLOAD_MSG, getRequestIdFromMDC(), payload);
        else
            log.warn(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }
    public static void warn(String msg){
        log.warn(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }

    public static void error(String msg, String payload){
        if(Boolean.TRUE.equals(printPayload))
            log.error(msg+ WITH_REQUEST_ID_AND_PAYLOAD_MSG, getRequestIdFromMDC(), payload);
        else
            log.error(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }
    public static void error(String msg){
        log.error(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }

    public static void trace(String msg, String payload){
        if(Boolean.TRUE.equals(printPayload))
            log.trace(msg+ WITH_REQUEST_ID_AND_PAYLOAD_MSG, getRequestIdFromMDC(), payload);
        else
            log.trace(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }
    public static void trace(String msg){
        log.trace(msg+WITH_REQUEST_ID_MSG, getRequestIdFromMDC());
    }

}
