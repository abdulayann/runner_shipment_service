package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.ViewsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IViewsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {ViewsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ViewsControllerTest {

    @Mock
    private IViewsService viewsService;
    @InjectMocks
    private ViewsController viewsController;

    @Test
    void createViewData() {
        // Mock
        when(viewsService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = viewsController.createViewData(new ViewsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createViewData2() {
        // Mock
        when(viewsService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = viewsController.createViewData(new ViewsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createViewData3() {
        // Mock
        when(viewsService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = viewsController.createViewData(new ViewsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(viewsService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = viewsController.update(new ViewsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(viewsService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = viewsController.update(new ViewsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void viewsController3() throws RunnerException {
        // Mock
        when(viewsService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = viewsController.update(new ViewsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws RunnerException {
        // Mock
        when(viewsService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = viewsController.delete(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() throws RunnerException {
        // Mock
        when(viewsService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = viewsController.retrieveById(111L, List.of());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() throws RunnerException {
        // Mock
        when(viewsService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = viewsController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }



}
