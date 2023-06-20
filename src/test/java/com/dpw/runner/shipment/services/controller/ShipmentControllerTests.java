package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.Pageable;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

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
        var sampleRequest = new ListCommonRequest();
        var samplePage = (ResponseEntity) createSamplePage();
        when(shipmentService.fetchShipments(CommonRequestModel.buildRequest(sampleRequest))).thenReturn(samplePage);
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

    private ResponseEntity<?> createSamplePage(){
        return ResponseHelper.buildListSuccessResponse(null,0,0);
    }


}
