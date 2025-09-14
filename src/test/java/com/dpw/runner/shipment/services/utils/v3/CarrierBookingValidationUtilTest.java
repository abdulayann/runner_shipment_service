package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class CarrierBookingValidationUtilTest {

    @Mock
    private IConsolidationV3Service consolidationV3Service;

    @InjectMocks
    private CarrierBookingValidationUtil carrierBookingValidationUtil;

    @Test
    void test_validateRequest_invalid(){
        String entityType = Constants.CONSOLIDATION;
        when(consolidationV3Service.findById(any())).thenThrow(new ValidationException("Ex"));

        assertThrows(ValidationException.class , () -> carrierBookingValidationUtil.validateRequest(entityType, 1L));
    }

    @Test
    void test_validateRequest_invalid1(){
        String entityType = Constants.SHIPMENT;
        assertThrows(ValidationException.class , () -> carrierBookingValidationUtil.validateRequest(entityType, 1L));
    }

    @Test
    void test_validateRequest(){
        String entityType = Constants.CONSOLIDATION;
        when(consolidationV3Service.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));

        assertNotNull(carrierBookingValidationUtil.validateRequest(entityType, 1L));
    }

    @Test
    void test_validateServiceType_invalid(){
        CarrierBookingRequest request = new CarrierBookingRequest();
        request.setServiceType("ST");

        assertThrows(ValidationException.class, () -> carrierBookingValidationUtil.validateServiceType(request));
    }

    @Test
    void test_validateServiceType(){
        CarrierBookingRequest request = new CarrierBookingRequest();
        request.setServiceType("P2P");

        carrierBookingValidationUtil.validateServiceType(request);
        Assertions.assertNotNull(request);
    }

}
