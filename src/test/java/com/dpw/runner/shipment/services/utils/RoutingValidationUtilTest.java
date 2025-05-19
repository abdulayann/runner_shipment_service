package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingValidationUtilTest {
    @Mock
    private IShipmentServiceV3 shipmentService;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private ICustomerBookingService customerBookingService;
    @InjectMocks
    private RoutingValidationUtil routingValidationUtil;

    @Test
    void testValidateUpdateRequest() {
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateUpdateRequest(null));
    }

    @Test
    void testValidateUpdateRequest1() {
        RoutingsRequest routingsRequest = new RoutingsRequest();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateUpdateRequest(routingsRequest));
    }


}