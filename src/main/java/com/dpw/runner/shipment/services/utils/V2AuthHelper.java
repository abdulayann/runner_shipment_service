package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_SERVICE_TYPE;

@Component
public class V2AuthHelper {

    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;

    public static final String SHIPMENT = "Shipment";

    public HttpHeaders getInvoiceServiceXApiKeyHeader() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(ApiConstants.X_ACCESS_TOKEN, billingServiceUrlConfig.getXApiKey());
        headers.add(SOURCE_SERVICE_TYPE, SHIPMENT);
        return headers;
    }

    public HttpHeaders getOrderManagementServiceSourceHeader() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(SOURCE_SERVICE_TYPE, SHIPMENT);
        return headers;
    }

}
