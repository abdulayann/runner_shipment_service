package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

@Execution(CONCURRENT)
@ExtendWith(MockitoExtension.class)
class V2AuthHelperTest {

    @InjectMocks
    private V2AuthHelper v2AuthHelper;

    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;

    @BeforeAll
    static void beforeAllSetup() {
    }

    @BeforeEach
    void setUp() {
    }

    @Test
    void testGetInvoiceServiceXApiKeyHeader() {
        Mockito.when(billingServiceUrlConfig.getXApiKey()).thenReturn("sampleXApiKey");

        HttpHeaders headers = v2AuthHelper.getInvoiceServiceXApiKeyHeader();
        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType());
        assertEquals("Shipment", headers.getFirst("SourceServiceType"));
        assertEquals("sampleXApiKey", headers.getFirst(ApiConstants.X_ACCESS_TOKEN));
    }

    @Test
    void testGetOrderManagementServiceSourceHeader() {
        HttpHeaders headers = v2AuthHelper.getOrderManagementServiceSourceHeader();
        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType());
        assertEquals("Shipment", headers.getFirst("SourceServiceType"));
    }
}