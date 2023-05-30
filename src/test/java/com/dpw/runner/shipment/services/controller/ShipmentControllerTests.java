package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.Pageable;
import com.dpw.runner.shipment.services.dto.response.RunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.IShipmentService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.HttpStatus;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class ShipmentControllerTests {

    @Mock
    IShipmentService shipmentService;

    @InjectMocks
    ShipmentController shipmentController;

    @BeforeEach
    public void setup(){
        initMocks(this);
    }


    @Test
    public void fetchByQuery_call_success(){
        var sampleRequest = Pageable.builder().build();
        var samplePage = createSamplePage();
        when(shipmentService.fetchShipments(sampleRequest)).thenReturn(samplePage);
        var response = shipmentController.fetchByQuery(sampleRequest);
        assertTrue(response.getBody().equals(samplePage));
        assertTrue(response.getStatusCode().equals(HttpStatus.OK));

    }

    @Test
    public void createTestRecord_call_success(){
        var sampleRequest = 1;
        var expectedTestShipmentsList = List.of(ShipmentDetails.builder().build());
        when(shipmentService.createTestShipment(sampleRequest)).thenReturn(expectedTestShipmentsList);
        var response = shipmentController.createTestRecord(sampleRequest);
        assertTrue(response.hasBody());
        assertTrue(response.getBody().equals(expectedTestShipmentsList));
        assertTrue(response.getStatusCode().equals(HttpStatus.OK));
    }

    private RunnerResponse createSamplePage(){
        return new RunnerResponse();
    }


}
