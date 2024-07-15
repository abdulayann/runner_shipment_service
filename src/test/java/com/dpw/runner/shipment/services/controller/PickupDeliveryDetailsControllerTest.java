package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {PickupDeliveryDetailsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PickupDeliveryDetailsControllerTest {

    @Mock
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @InjectMocks
    private PickupDeliveryDetailsController pickupDeliveryDetailsController;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testCreate_Success() {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        ResponseEntity<IRunnerResponse> response = new ResponseEntity<>(HttpStatus.OK);

        when(pickupDeliveryDetailsService.create(any(CommonRequestModel.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.create(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).create(any(CommonRequestModel.class));
    }

    @Test
    void testCreate_Exception() {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();

        when(pickupDeliveryDetailsService.create(any(CommonRequestModel.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.create(request);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).create(any(CommonRequestModel.class));
    }

    @Test
    void testDelete_Success() {
        Long id = 1L;
        ResponseEntity<IRunnerResponse> response = new ResponseEntity<>(HttpStatus.OK);

        when(pickupDeliveryDetailsService.delete(any(CommonRequestModel.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.delete(id);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).delete(any(CommonRequestModel.class));
    }

    @Test
    void testList_Success() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        ResponseEntity<IRunnerResponse> response = new ResponseEntity<>(HttpStatus.OK);

        when(pickupDeliveryDetailsService.list(any(CommonRequestModel.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.list(listCommonRequest);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).list(any(CommonRequestModel.class));
    }

    @Test
    void testRetrieveById_Success() {
        Long id = 1L;
        ResponseEntity<IRunnerResponse> response = new ResponseEntity<>(HttpStatus.OK);

        when(pickupDeliveryDetailsService.retrieveById(any(CommonRequestModel.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.retrieveById(id);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).retrieveById(any(CommonRequestModel.class));
    }

    @Test
    void testUpdate_Success() throws RunnerException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        ResponseEntity<IRunnerResponse> response = new ResponseEntity<>(HttpStatus.OK);

        when(pickupDeliveryDetailsService.update(any(CommonRequestModel.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.update(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).update(any(CommonRequestModel.class));
    }

    @Test
    void testUpdate_Exception() throws RunnerException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();

        when(pickupDeliveryDetailsService.update(any(CommonRequestModel.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> result = pickupDeliveryDetailsController.update(request);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        verify(pickupDeliveryDetailsService, times(1)).update(any(CommonRequestModel.class));
    }
}