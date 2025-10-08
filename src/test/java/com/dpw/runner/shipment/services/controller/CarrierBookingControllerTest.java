package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SyncBookingToService;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CarrierBookingControllerTest {

    @Mock
    private ICarrierBookingService carrierBookingService;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private CarrierBookingController carrierBookingController;

    @Test
    void create() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(carrierBookingService.create(any(CarrierBookingRequest.class))).thenReturn(new CarrierBookingResponse());

        // Test
        var responseEntity = carrierBookingController.create(new CarrierBookingRequest());

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void retrieveById() {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(carrierBookingService.retrieveById(anyLong())).thenReturn(new CarrierBookingResponse());

        // Test
        var responseEntity = carrierBookingController.retrieveById(1L);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void list()  {
        // Mock
        when(carrierBookingService.list(any(), eq(true))).thenReturn(ResponseEntity.ok().build());

        // Test
        var responseEntity = carrierBookingController.list(new ListCommonRequest(), true);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list_WithoutMasterData() {
        // Mock
        when(carrierBookingService.list(any(), eq(false))).thenReturn(ResponseEntity.ok().build());

        // Test
        var responseEntity = carrierBookingController.list(new ListCommonRequest(), false);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void update() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(carrierBookingService.update(any(CarrierBookingRequest.class))).thenReturn(new CarrierBookingResponse());

        // Test
        var responseEntity = carrierBookingController.update(new CarrierBookingRequest());

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void delete() {
        // Mock
        doNothing().when(carrierBookingService).delete(anyLong());

        // Test
        var responseEntity = carrierBookingController.delete(1L);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void getAllMasterData() {
        // Mock
        when(carrierBookingService.getAllMasterData(anyLong())).thenReturn(ResponseEntity.ok().build());

        // Test
        var responseEntity = carrierBookingController.getAllMasterData(1L);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData_Exception() {
        // Mock
        when(carrierBookingService.getAllMasterData(anyLong())).thenThrow(new ValidationException("Service error"));

        // Test
        assertNotNull(carrierBookingController.getAllMasterData(1L));

    }


    @Test
    void syncCarrierBookingToService() throws RunnerException {
        // Mock
        doNothing().when(carrierBookingService).syncCarrierBookingToService(any(SyncBookingToService.class));

        // Test
        var responseEntity = carrierBookingController.syncCarrierBookingToService(new SyncBookingToService());

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDefault_Success() {
        // Mock
        Long entityId = 123L;
        EntityType type = EntityType.CONSOLIDATION; // Replace with actual enum value
        CarrierBookingResponse mockResponse = new CarrierBookingResponse();

        when(carrierBookingService.getDefaultCarrierBookingValues(type, entityId))
                .thenReturn(mockResponse);

        // Test
        ResponseEntity<IRunnerResponse> responseEntity = carrierBookingController.getDefault(entityId, type);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        verify(carrierBookingService).getDefaultCarrierBookingValues(type, entityId);
    }

    @Test
    void getDefault_Exception_WithMessage() {
        // Mock
        Long entityId = 123L;
        EntityType type = EntityType.CONSOLIDATION; // Replace with actual enum value
        String errorMessage = "Custom error message";

        when(carrierBookingService.getDefaultCarrierBookingValues(type, entityId))
                .thenThrow(new RuntimeException(errorMessage));

        // Test
        carrierBookingController.getDefault(entityId, type);

        // Assert
        verify(carrierBookingService).getDefaultCarrierBookingValues(type, entityId);
    }

    @Test
    void getDefault_Exception_WithoutMessage() {
        // Mock
        Long entityId = 123L;
        EntityType type = EntityType.CONSOLIDATION; // Replace with actual enum value

        when(carrierBookingService.getDefaultCarrierBookingValues(type, entityId))
                .thenThrow(new RuntimeException((String) null));

        // Test
        carrierBookingController.getDefault(entityId, type);

        // Assert
        verify(carrierBookingService).getDefaultCarrierBookingValues(type, entityId);
    }

    @Test
    void cancel_Success() {
        // Mock
        Long id = 123L;
        doNothing().when(carrierBookingService).cancel(id);

        // Test
        ResponseEntity<IRunnerResponse> responseEntity = carrierBookingController.cancel(id);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        verify(carrierBookingService).cancel(id);
    }

    @Test
    void submitOrAmend_Success() throws RunnerException {
        SubmitAmendInttraRequest submitAmendInttraRequest = new SubmitAmendInttraRequest();
        doNothing().when(carrierBookingService).submitOrAmend(submitAmendInttraRequest);

        ResponseEntity<IRunnerResponse> responseEntity = carrierBookingController.submitOrAmend(submitAmendInttraRequest);

        assertNotNull(responseEntity);
    }
    @Test
    void consolidatedList_WithoutMasterData() {
        // Mock
        when(carrierBookingService.consolidatedList(any(), eq(false))).thenReturn(ResponseEntity.ok().build());

        // Test
        var responseEntity = carrierBookingController.consolidatedList(new ListCommonRequest(), false);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void cloneBooking_Exception_WithMessage() {
        // Mock
        Long carrierBookingId = 123L;
        String errorMessage = "Custom error message";

        when(carrierBookingService.cloneBooking(carrierBookingId))
                .thenThrow(new RuntimeException(errorMessage));

        // Test
        carrierBookingController.cloneCarrierBooking(carrierBookingId);

        // Assert
        verify(carrierBookingService).cloneBooking(carrierBookingId);
    }
}
