package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
@Component
public class PackingValidationV3Util {

    public void validateUpdateBulkRequest(List<PackingV3Request> requests, List<Packing> existingPackings) {
        List<Long> incomingIds = requests.stream()
                .map(PackingV3Request::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        Set<Long> existingIds = existingPackings.stream()
                .map(Packing::getId)
                .collect(Collectors.toSet());

        // Validate: If any ID in the request is not found in DB, throw exception
        List<Long> missingIds = incomingIds.stream()
                .filter(id -> !existingIds.contains(id))
                .toList();

        if (!missingIds.isEmpty()) {
            throw new DataRetrievalFailureException("No packing found for the ids: " + missingIds);
        }
    }

    public void validateUpdateRequest(PackingV3Request request) throws RunnerException {
        if (request == null || request.getId() == null) {
            throw new RunnerException("Packing Id cannot be null or empty.");
        }
    }

    public void validateDeleteBulkRequest(List<PackingV3Request> requests) {
        List<PackingV3Request> missingIdRequests = requests.stream()
                .filter(req -> req.getId() == null)
                .toList();

        if (!missingIdRequests.isEmpty()) {
            throw new DataRetrievalFailureException("All packing delete requests must have a id.");
        }
    }

}
