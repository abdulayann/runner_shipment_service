package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.authservice.ApiKeyService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ApiKeyServiceTest {

    private ApiKeyService apiKeyService;

    @BeforeEach
    void setUp() {
        Set<String> validKeys = new HashSet<>();
        validKeys.add("key123");
        validKeys.add("secureKey456");

        apiKeyService = new ApiKeyService(validKeys);
    }

    @Test
    void testValidApiKey_ShouldReturnTrue() {
        assertTrue(apiKeyService.isValidApiKey("key123"));
        assertTrue(apiKeyService.isValidApiKey("secureKey456"));
    }

    @Test
    void testInvalidApiKey_ShouldReturnFalse() {
        assertFalse(apiKeyService.isValidApiKey("wrongKey"));
    }

    @Test
    void testNullApiKey_ShouldReturnFalse() {
        assertFalse(apiKeyService.isValidApiKey(null));
    }

    @Test
    void testEmptyApiKey_ShouldReturnFalse() {
        assertFalse(apiKeyService.isValidApiKey(""));
    }

    @Test
    void testWhitespaceApiKey_ShouldReturnFalse() {
        assertFalse(apiKeyService.isValidApiKey("   "));
    }
}
