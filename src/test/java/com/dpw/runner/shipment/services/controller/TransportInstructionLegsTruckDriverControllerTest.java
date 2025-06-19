package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsTruckDriverRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsTruckDriverService;
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

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TransportInstructionLegsTruckDriverControllerTest {

    @Mock
    private ITransportInstructionLegsTruckDriverService transportInstructionLegsTruckDriverService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private TransportInstructionTruckDriverController transportInstructionTruckDriverController;


    @Test
    void create() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsTruckDriverRequest request = new TransportInstructionLegsTruckDriverRequest();

        // Mock
        var responseEntity = transportInstructionTruckDriverController.create(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsTruckDriverRequest request = new TransportInstructionLegsTruckDriverRequest();
        request.setId(1l);
        // Mock
        var responseEntity = transportInstructionTruckDriverController.update(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        var responseEntity = transportInstructionTruckDriverController.delete(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() {
        // Mock
        var responseEntity = transportInstructionTruckDriverController.retrieveById(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        TransportInstructionLegsTruckDriverListResponse legsListResponse = new TransportInstructionLegsTruckDriverListResponse();
        legsListResponse.setTotalCount(1l);
        legsListResponse.setTotalPages(1);
        legsListResponse.setTiLegsTruckDriverResponses(List.of(new TransportInstructionLegsTruckDriverResponse()));
        when(transportInstructionLegsTruckDriverService.list(any())).thenReturn(legsListResponse);
        // Mock
        var responseEntity = transportInstructionTruckDriverController.list(new ListCommonRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
