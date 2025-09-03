package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsReferenceRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsReferenceService;
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
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionReferenceController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TransportInstructionLegsReferenceControllerTest {

    @Mock
    private ITransportInstructionLegsReferenceService transportInstructionLegsReferenceService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private TransportInstructionReferenceController transportInstructionReferenceController;


    @Test
    void create() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsReferenceRequest request = new TransportInstructionLegsReferenceRequest();

        // Mock
        var responseEntity = transportInstructionReferenceController.create(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsReferenceRequest request = new TransportInstructionLegsReferenceRequest();
        request.setId(1l);
        // Mock
        var responseEntity = transportInstructionReferenceController.update(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        var responseEntity = transportInstructionReferenceController.delete(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() {
        // Mock
        var responseEntity = transportInstructionReferenceController.retrieveById(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list()  {
        TransportInstructionLegsReferenceListResponse legsListResponse = new TransportInstructionLegsReferenceListResponse();
        legsListResponse.setTotalCount(1l);
        legsListResponse.setTotalPages(1);
        legsListResponse.setTiLegsReferenceResponses(List.of(new TransportInstructionLegsReferenceResponse()));
        when(transportInstructionLegsReferenceService.list(any(), anyBoolean())).thenReturn(legsListResponse);
        // Mock
        var responseEntity = transportInstructionReferenceController.list(new ListCommonRequest(), true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
