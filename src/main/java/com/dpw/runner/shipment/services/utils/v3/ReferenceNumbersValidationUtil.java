package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ReferenceNumbersValidationUtil {

    public void validateUpdateRequest(ReferenceNumbersRequest request) {
        if (request == null) {
            throw new IllegalArgumentException("Update request cannot be null or empty.");
        }

        if (request.getId() == null) {
            throw new IllegalArgumentException("Reference Number ID is missing for item. All items must have a valid ID.");
        }
    }
}
