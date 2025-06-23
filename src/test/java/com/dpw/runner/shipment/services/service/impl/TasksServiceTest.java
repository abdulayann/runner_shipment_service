package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TasksServiceTest {

    @Mock
    private IV1Service iv1Service;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private TasksService tasksService;

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
    }

    @Test
    void testCreate() {
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.create(CommonRequestModel.builder().build());
        assertNull(responseEntity);
    }

    @Test
    void testUpdate() {
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.update(CommonRequestModel.builder().build());
        assertNull(responseEntity);
    }

    @Test
    void testList() {
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.list(CommonRequestModel.builder().build());
        assertNull(responseEntity);
    }

    @Test
    void testListAsync() {
        var responseEntity = tasksService.listAsync(CommonRequestModel.builder().build());
        assertNull(responseEntity);
    }

    @Test
    void testDelete() {
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.delete(CommonRequestModel.builder().build());
        assertNull(responseEntity);
    }

    @Test
    void testRetrieveById() {
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.retrieveById(CommonRequestModel.builder().build());
        assertNull(responseEntity);
    }

    @Test
    void testCreateTaskForHbl() {
        when(iv1Service.createTaskforHBL(any())).thenReturn(new HblTaskCreationResponse());
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.createTaskForHbl(CommonRequestModel.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveTask_Console() {
        var mockInput = CommonGetRequest.builder().id(11L).build();
        TaskResponse taskResponse = TaskResponse.builder().entityType(Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS).taskType("9").build();
        when(iv1Service.retrieveTask(any())).thenReturn(V1RetrieveResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(TaskResponse.class))).thenReturn(taskResponse);
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.retrieveTask(CommonRequestModel.buildRequest(mockInput));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void testRetrieveTask_Shipment() {
        var mockInput = CommonGetRequest.builder().id(11L).build();
        TaskResponse taskResponse = TaskResponse.builder().entityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS).taskType("1").build();
        when(iv1Service.retrieveTask(any())).thenReturn(V1RetrieveResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(TaskResponse.class))).thenReturn(taskResponse);
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.retrieveTask(CommonRequestModel.buildRequest(mockInput));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateTask() {
        var mockInput = TaskUpdateRequest.builder().build();
        when(iv1Service.updateTask(any())).thenReturn(V1DataResponse.builder().build());
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.updateTask(CommonRequestModel.buildRequest(mockInput));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createTask() {
        var mockInput = TaskCreateRequest.builder().build();
        when(iv1Service.createTask(any())).thenReturn(TaskCreateResponse.builder().build());
        ResponseEntity<IRunnerResponse> responseEntity = tasksService.createTask(CommonRequestModel.buildRequest(mockInput));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


}