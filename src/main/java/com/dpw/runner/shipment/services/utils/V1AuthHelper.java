package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_SERVICE_TYPE;

@Component
public class V1AuthHelper {

    public static final String SHIPMENT = "Shipment";
    @Value("${v1service.dataSync.xApiKey}")
    private String xApiKey;

    public static HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("Authorization", RequestAuthContext.getAuthToken());
        headers.add(LoggingConstants.REQUEST_ID, LoggerHelper.getRequestIdFromMDC());
        headers.add(SOURCE_SERVICE_TYPE, SHIPMENT);
        return headers;
    }

    public HttpHeaders getHeadersForDataSync() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(ApiConstants.X_API_KEY, xApiKey);
        headers.add("X-USER-NAME", UserContext.getUser().getUsername());
        headers.add("X-TENANT-ID", StringUtility.convertToString(UserContext.getUser().getTenantId()));
        headers.add("X-SYNC-REQUEST", "true");
        headers.add(SOURCE_SERVICE_TYPE, SHIPMENT);
        headers.add(LoggingConstants.REQUEST_ID, LoggerHelper.getRequestIdFromMDC());
        return headers;
    }

    public HttpHeaders getHeadersForDataSyncFromKafka(String userName, Integer tenantId, String updateUsername) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(ApiConstants.X_API_KEY, xApiKey);
        headers.add("X-USER-NAME", userName);
        headers.add("X-UPDATE-USER", updateUsername);
        headers.add("X-TENANT-ID", StringUtility.convertToString(tenantId));
        headers.add("X-SYNC-REQUEST", "true");
        headers.add(SOURCE_SERVICE_TYPE, SHIPMENT);
        headers.add(LoggingConstants.REQUEST_ID, LoggerHelper.getRequestIdFromMDC());
        return headers;
    }
}
