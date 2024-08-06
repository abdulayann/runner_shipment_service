package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;

@Component
public class V1AuthHelper {

    @Value("${v1service.dataSync.xApiKey}")
    private String xApiKey;

    public static HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("Authorization", RequestAuthContext.getAuthToken());
        headers.add("SourceServiceType", "Shipment");
        return headers;
    }

    public HttpHeaders getHeadersForDataSync() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(ApiConstants.X_API_KEY, xApiKey);
        headers.add("X-USER-NAME", UserContext.getUser().getUsername());
        headers.add("X-TENANT-ID", StringUtility.convertToString(UserContext.getUser().getTenantId()));
        headers.add("X-SYNC-REQUEST", "true");
        headers.add("SourceServiceType", "Shipment");
        return headers;
    }
    
    public HttpHeaders getHeadersForDataSyncFromKafka(String userName, Integer tenantId) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(ApiConstants.X_API_KEY, xApiKey);
        headers.add("X-USER-NAME", userName);
        headers.add("X-TENANT-ID", StringUtility.convertToString(tenantId));
        headers.add("SourceServiceType", "Shipment");
        headers.add("X-SYNC-REQUEST", "true");
        return headers;
    }
}
