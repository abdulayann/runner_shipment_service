package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

@Component
public class V1AuthHelper {

    @Value("${v1service.dataSync.xApiKey}")
    private String X_API_KEY;
    @Autowired
    private ContextUtility contextUtility;

    public HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("Authorization", contextUtility.requestAuthContext.getAuthToken());
        return headers;
    }

    public HttpHeaders getHeadersForDataSync() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("x-api-key", X_API_KEY);
        headers.add("X-USER-NAME", contextUtility.userContext.getUser().getUsername());
        headers.add("X-TENANT-ID", StringUtility.convertToString(contextUtility.userContext.getUser().getTenantId()));
        return headers;
    }
}
