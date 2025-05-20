package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
        when(cargoService.getCargoDetails(any())).thenReturn(new CargoDetailsResponse());
        var responseEntity = cargoController.getCargoDetails(new CargoDetailsRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
}
