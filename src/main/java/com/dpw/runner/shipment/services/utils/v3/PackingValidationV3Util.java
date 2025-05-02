package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
public class PackingValidationV3Util {

    public void validateUpdateBulkRequest(List<PackingV3Request> requests) {
        if (requests == null || requests.isEmpty()) {
            throw new IllegalArgumentException("Bulk update request cannot be null or empty.");
        }

        for (int index = 0; index < requests.size(); index++) {
            PackingV3Request packing = requests.get(index);
            if (packing.getId() == null) {
                throw new IllegalArgumentException(
                        String.format("Packing ID is missing for item at index %d. All items must have a valid ID.", index)
                );
            }
        }
    }

    public void validateUpdateRequest(PackingV3Request request) {
        if (request == null || request.getId() == null) {
            throw new IllegalArgumentException("Packing Id cannot be null or empty.");
        }
    }

}
