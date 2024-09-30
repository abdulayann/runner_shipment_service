package com.dpw.runner.booking.services.utils;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.booking.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.booking.services.dto.request.UsersDto;
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
}