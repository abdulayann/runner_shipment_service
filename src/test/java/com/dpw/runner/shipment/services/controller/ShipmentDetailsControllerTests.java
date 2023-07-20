package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.MockitoAnnotations.initMocks;

public class ShipmentDetailsControllerTests {

    @Mock
    IShipmentService shipmentService;

    @InjectMocks
    ShipmentDetailsController shipmentDetailsController;

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
