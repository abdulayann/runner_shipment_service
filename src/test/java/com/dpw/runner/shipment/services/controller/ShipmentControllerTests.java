package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
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
    public void setup() {
        initMocks(this);
    }


    @Test
    public void fetchByQuery_call_success() {

    }

    @Test
    public void createTestRecord_call_success() {
    }

    private ResponseEntity<?> createSamplePage() {
        return ResponseHelper.buildListSuccessResponse(null, 0, 0);
    }


}
