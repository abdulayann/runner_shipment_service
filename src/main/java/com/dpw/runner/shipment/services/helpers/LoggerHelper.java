package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import org.slf4j.MDC;

import java.util.UUID;

public class LoggerHelper {
    public static String getRequestIdFromMDC(){
        return MDC.get(LoggingConstants.REQUEST_ID);
    }

    public static void putRequestId(String requestId) {
        if(requestId == null || requestId.isEmpty()) {
            requestId = UUID.randomUUID().toString();
        }
        MDC.put(LoggingConstants.REQUEST_ID, requestId);
    }
}
