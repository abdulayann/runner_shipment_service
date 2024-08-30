package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@TestPropertySource("classpath:application-test.properties")
@Execution(CONCURRENT)
class V1AuthHelperTest {

    private static V1AuthHelper v1AuthHelper;

    @Value("${v1service.dataSync.xApiKey}")
    private String xApiKey;

    @BeforeAll
    static void beforeAllSetup() {
        RequestAuthContext.setAuthToken("test-auth-token");
        UserContext.setUser(UsersDto.builder().TenantId(123).Username("test-user").build());
    }

    @BeforeEach
    void setUp() {
        v1AuthHelper = new V1AuthHelper();
    }

    @Test
    void testGetHeaders() {
        HttpHeaders headers = V1AuthHelper.getHeaders();
        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType());
        assertEquals("test-auth-token", headers.getFirst("Authorization"));
    }

    @Test
    void testGetHeadersForDataSync() {
        HttpHeaders headers = v1AuthHelper.getHeadersForDataSync();
        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType());
        assertEquals(null, headers.getFirst(ApiConstants.X_API_KEY));
        assertEquals("test-user", headers.getFirst("X-USER-NAME"));
        assertEquals("123", headers.getFirst("X-TENANT-ID"));
    }

    @Test
    void testGetHeadersForDataSyncFromKafka() {
        HttpHeaders headers = v1AuthHelper.getHeadersForDataSyncFromKafka("kafka-user", 456, null);
        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType());
        assertEquals(null, headers.getFirst(ApiConstants.X_API_KEY));
        assertEquals("kafka-user", headers.getFirst("X-USER-NAME"));
        assertEquals("456", headers.getFirst("X-TENANT-ID"));
    }
}