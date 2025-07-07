package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsPackagesService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionPackagesController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TransportInstructionLegsPackagesControllerTest {

    @Mock
    private ITransportInstructionLegsPackagesService transportInstructionLegsPackagesService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private TransportInstructionPackagesController transportInstructionPackagesController;


    @Test
    void create() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsPackagesRequest request = new TransportInstructionLegsPackagesRequest();
        // Mock
        var responseEntity = transportInstructionPackagesController.create(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsPackagesRequest request = new TransportInstructionLegsPackagesRequest();

        // Mock
        var responseEntity = transportInstructionPackagesController.update(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        var responseEntity = transportInstructionPackagesController.delete(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() {
        // Mock
        var responseEntity = transportInstructionPackagesController.retrieveById(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        TransportInstructionLegsPackagesListResponse legsListResponse = new TransportInstructionLegsPackagesListResponse();
        legsListResponse.setTotalCount(1l);
        legsListResponse.setTotalPages(1);
        legsListResponse.setTiLegsPackagesResponses(List.of(new TransportInstructionLegsPackagesResponse()));
        when(transportInstructionLegsPackagesService.list(any())).thenReturn(legsListResponse);
        // Mock
        var responseEntity = transportInstructionPackagesController.list(new ListCommonRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
