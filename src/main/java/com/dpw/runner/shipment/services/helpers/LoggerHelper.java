package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.slf4j.MDC;

import java.util.UUID;

public class LoggerHelper {
    public static String getRequestIdFromMDC(){
        return MDC.get(LoggingConstants.REQUEST_ID);
    }

    public static void putRequestId(String requestId) {
        if(StringUtility.isEmpty(requestId)) {
            requestId = UUID.randomUUID().toString();
        }
        MDC.put(LoggingConstants.REQUEST_ID, requestId);
    }
}
