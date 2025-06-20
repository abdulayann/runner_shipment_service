package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsContainersService;
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

@ContextConfiguration(classes = {TransportInstructionContainersController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TransportInstructionLegsContainersControllerTest {

    @Mock
    private ITransportInstructionLegsContainersService transportInstructionLegsContainersService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private TransportInstructionContainersController transportInstructionContainersController;


    @Test
    void create() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsContainersRequest request = new TransportInstructionLegsContainersRequest();
        // Mock
        var responseEntity = transportInstructionContainersController.create(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsContainersRequest request = new TransportInstructionLegsContainersRequest();

        // Mock
        var responseEntity = transportInstructionContainersController.update(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        var responseEntity = transportInstructionContainersController.delete(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() {
        // Mock
        var responseEntity = transportInstructionContainersController.retrieveById(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        TransportInstructionLegsContainersListResponse legsListResponse = new TransportInstructionLegsContainersListResponse();
        legsListResponse.setTotalCount(1l);
        legsListResponse.setTotalPages(1);
        legsListResponse.setTiLegsContainersResponses(List.of(new TransportInstructionLegsContainersResponse()));
        when(transportInstructionLegsContainersService.list(any())).thenReturn(legsListResponse);
        // Mock
        var responseEntity = transportInstructionContainersController.list(new ListCommonRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
