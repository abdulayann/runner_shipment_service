package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.utils.v3.PartiesValidationUtil;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class PartiesValidationUtilTest {

    @Spy
    private PartiesValidationUtil partiesValidationUtil;
    @Test
    void validateUpdateRequest_shouldThrowException_whenRequestIsNull() {
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                partiesValidationUtil.validateUpdateRequest(null));
        assertEquals("Update request cannot be null or empty.", exception.getMessage());
    }
    @Test
    void validateUpdateRequest_shouldThrowException_whenRequestIdIsNull() {
        PartiesRequest request = new PartiesRequest(); // assuming default constructor
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                partiesValidationUtil.validateUpdateRequest(request));
        assertEquals("Party ID is missing for item. All items must have a valid ID.", exception.getMessage());
    }
    @Test
    void validateUpdateRequest_shouldPass_whenRequestIsValid() {
        PartiesRequest request = new PartiesRequest();
        request.setId(123L); // set a valid ID
        assertDoesNotThrow(() -> partiesValidationUtil.validateUpdateRequest(request));
    }
}
