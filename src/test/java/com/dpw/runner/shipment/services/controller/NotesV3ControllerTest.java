package com.dpw.runner.shipment.services.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INotesV3Service;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = {NotesV3Controller.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotesV3ControllerTest {

    @Mock
    private INotesV3Service notesService;
    @InjectMocks
    private NotesV3Controller notesController;

    @Test
    void create() {
        // Mock
        when(notesService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = notesController.create(NotesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(notesService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = notesController.create(NotesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() {
        // Mock
        when(notesService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = notesController.create(NotesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(notesService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = notesController.update(NotesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(notesService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = notesController.update(NotesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(notesService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = notesController.update(NotesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws RunnerException {
        // Mock
        when(notesService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = notesController.delete(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() throws RunnerException {
        // Mock
        when(notesService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = notesController.retrieve(1111L, List.of());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() throws RunnerException {
        // Mock
        when(notesService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = notesController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


}
