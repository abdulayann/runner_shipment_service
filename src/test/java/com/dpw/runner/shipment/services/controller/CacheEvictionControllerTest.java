package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.CacheRequest;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import com.dpw.runner.shipment.services.service.impl.CacheEvictionService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {CacheEvictionController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CacheEvictionControllerTest {

    @Mock
    private CacheEvictionService cacheEviction;
    @Mock
    private ApiKeyAuthenticationService authenticationService;
    @InjectMocks
    private CacheEvictionController cacheEvictionController;

    @Test
    void evictAllCache() {
        // Mock
        doNothing().when(authenticationService).authenticate(anyString(), anyString());
        doNothing().when(cacheEviction).clearAllCache();
        // Test
        var responseEntity = cacheEvictionController.evictAllCache(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void evictAllCache2() {
        // Mock
        doNothing().when(authenticationService).authenticate(anyString(), anyString());
        doThrow(new RuntimeException("RuntimeException")).when(cacheEviction).clearAllCache();
        // Test
        var responseEntity = cacheEvictionController.evictAllCache(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void evictAllCache3() {
        // Mock
        doNothing().when(authenticationService).authenticate(anyString(), anyString());
        doThrow(new RuntimeException()).when(cacheEviction).clearAllCache();
        // Test
        var responseEntity = cacheEvictionController.evictAllCache(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void evictCache() {
        // Mock
        doNothing().when(authenticationService).authenticate(anyString(), anyString());
        doNothing().when(cacheEviction).clearCacheByName(any());
        // Test
        var responseEntity = cacheEvictionController.evictCache(new CacheRequest(), UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void evictCache2() {
        // Mock
        doNothing().when(authenticationService).authenticate(anyString(), anyString());
        doThrow(new RuntimeException("RuntimeException")).when(cacheEviction).clearCacheByName(any());
        // Test
        var responseEntity = cacheEvictionController.evictCache(new CacheRequest(), UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void evictCache3() {
        // Mock
        doNothing().when(authenticationService).authenticate(anyString(), anyString());
        doThrow(new RuntimeException()).when(cacheEviction).clearCacheByName(any());
        // Test
        var responseEntity = cacheEvictionController.evictCache(new CacheRequest(), UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }



}
