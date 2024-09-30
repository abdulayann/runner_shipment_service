package com.dpw.runner.booking.services.utils;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.booking.services.commons.constants.LoggingConstants;
import com.dpw.runner.booking.services.helpers.LoggerHelper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;

@Component
public class V1AuthHelper {

    @Value("${v1service.dataSync.xApiKey}")
    private String xApiKey;

    public static final String SOURCE_SERVICE_TYPE = "SourceServiceType";
    public static final String SHIPMENT = "Shipment";

    public static HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("Authorization", RequestAuthContext.getAuthToken());
        headers.add(LoggingConstants.REQUEST_ID, LoggerHelper.getRequestIdFromMDC());
        headers.add(SOURCE_SERVICE_TYPE, SHIPMENT);
        return headers;
    }
}
