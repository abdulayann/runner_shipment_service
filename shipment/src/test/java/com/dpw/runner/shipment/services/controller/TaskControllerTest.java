package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITasksService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TaskController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TaskControllerTest {

    @Mock
    private ITasksService tasksService;
    @InjectMocks
    private TaskController taskController;

    @Test
    void create() {
        // Mock
        when(tasksService.createTaskForHbl(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = taskController.create(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(tasksService.createTaskForHbl(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = taskController.create(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() {
        // Mock
        when(tasksService.createTaskForHbl(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = taskController.create(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() {
        // Mock
        when(tasksService.retrieveTask(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = taskController.retrieve(1L);
        // Assert
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() {
        // Mock
        when(tasksService.retrieveTask(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = taskController.retrieve(1L);
        // Assert
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() {
        // Mock
        when(tasksService.retrieveTask(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = taskController.retrieve(1L);
        // Assert
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }



}
