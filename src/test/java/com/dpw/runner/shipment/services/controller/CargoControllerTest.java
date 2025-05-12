package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.ContainerDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.impl.CargoService;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {CargoController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class CargoControllerTest {

    @Mock
    private ICargoService cargoService;

    @InjectMocks
    private CargoController cargoController;
    
    @Test
    void getContainerDetailsSuccess() throws RunnerException {
        when(cargoService.getContainerDetails(any())).thenReturn(new ContainerDetailsResponse());
        var responseEntity = cargoController.getContainerDetails(new ContainerDetailsRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
}
