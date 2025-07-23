package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsService;
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
import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionLegsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TransportInstructionLegsControllerTest {

    @Mock
    private ITransportInstructionLegsService transportInstructionLegsService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private TransportInstructionLegsController transportInstructionLegsController;


    @Test
    void create() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsRequest request = new TransportInstructionLegsRequest();
        request.setLegType(TILegType.EMPTY);
        request.setTiId(1l);
        request.setRemarks("remarks");
        request.setSequence(1L);
        request.setEstimatedPickup(LocalDateTime.now());
        request.setActualPickup(LocalDateTime.now());
        request.setActualDelivery(LocalDateTime.now().plusDays(3));
        request.setEstimatedDelivery(LocalDateTime.now().plusDays(3));
        // Mock
        var responseEntity = transportInstructionLegsController.create(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsRequest request = new TransportInstructionLegsRequest();
        request.setId(1l);
        request.setLegType(TILegType.EMPTY);
        request.setTiId(1l);
        request.setRemarks("remarks");
        request.setSequence(1L);
        request.setEstimatedPickup(LocalDateTime.now());
        request.setActualPickup(LocalDateTime.now());
        request.setActualDelivery(LocalDateTime.now().plusDays(3));
        request.setEstimatedDelivery(LocalDateTime.now().plusDays(3));
        // Mock
        var responseEntity = transportInstructionLegsController.update(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws IOException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        var responseEntity = transportInstructionLegsController.delete(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() {
        // Mock
        var responseEntity = transportInstructionLegsController.retrieveById(1l);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        TransportInstructionLegsListResponse legsListResponse = new TransportInstructionLegsListResponse();
        legsListResponse.setTotalCount(1l);
        legsListResponse.setTotalPages(1);
        legsListResponse.setTiLegsResponses(List.of(new TransportInstructionLegsResponse()));
        when(transportInstructionLegsService.list(any(), anyBoolean(), anyBoolean())).thenReturn(legsListResponse);
        // Mock
        var responseEntity = transportInstructionLegsController.list(new ListCommonRequest(), true, true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
