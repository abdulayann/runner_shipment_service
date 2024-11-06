package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.EVgmRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEVgmService;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.lang.reflect.InvocationTargetException;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EVgmControllerTest {
    @Mock
    private IEVgmService eVgmService;
    @InjectMocks
    private EVgmController eVgmController;

    @Test
    void create() throws RunnerException {
        // Mock
        when(eVgmService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eVgmController.create(EVgmRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() throws RunnerException {
        // Mock
        when(eVgmService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eVgmController.create(EVgmRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() throws RunnerException {
        // Mock
        when(eVgmService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eVgmController.create(EVgmRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void update() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(eVgmService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eVgmController.update(EVgmRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(eVgmService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eVgmController.update(EVgmRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(eVgmService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eVgmController.update(EVgmRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(eVgmService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eVgmController.retrieveById(Optional.of(111L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        // Mock
        when(eVgmService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eVgmController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(eVgmService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eVgmController.delete(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
}
