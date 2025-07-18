package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

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

    public void validateUpdateBulkRequest(List<ReferenceNumbersRequest> requests) {
        if (requests == null || requests.isEmpty()) {
            throw new IllegalArgumentException("Bulk update request cannot be null or empty.");
        }

        for (int index = 0; index < requests.size(); index++) {
            ReferenceNumbersRequest container = requests.get(index);
            if (container.getId() == null) {
                throw new IllegalArgumentException(
                        String.format("Reference number ID is missing for item at index %d. All items must have a valid ID.", index)
                );
            }
        }
    }
}
