package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.entity.Routings;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;


@Component
public class RoutingValidationUtil {
    public void validateUpdateRequest(RoutingsRequest request) {
        if (request == null) {
            throw new IllegalArgumentException("Update request cannot be null or empty.");
        }
        if (request.getId() == null) {
            throw new IllegalArgumentException("Routing ID is missing.");
        }
    }

    public void validateDeleteRequest(CommonGetRequest request) {
        if (request == null) {
            throw new IllegalArgumentException("Delete request cannot be null or empty.");
        }
        if (request.getId() == null) {
            throw new IllegalArgumentException("Routing ID is missing.");
        }
    }

    public void validateUpdateBulkRequest(List<RoutingsRequest> routingListRequest, List<Routings> existingRoutings) {
        List<Long> incomingIds = routingListRequest.stream()
                .map(RoutingsRequest::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();
        Set<Long> existingIds = existingRoutings.stream()
                .map(Routings::getId)
                .collect(Collectors.toSet());
        // Validate: If any ID in the request is not found in DB, throw exception
        List<Long> missingIds = incomingIds.stream()
                .filter(id -> !existingIds.contains(id))
                .toList();
        if (!missingIds.isEmpty()) {
            throw new DataRetrievalFailureException("No Routing found for the ids: " + missingIds);
        }
    }

    public void validateDeleteBulkRequest(List<RoutingsRequest> requests) {
        List<RoutingsRequest> missingIdRequests = requests.stream()
                .filter(req -> req.getId() == null)
                .toList();

        if (!missingIdRequests.isEmpty()) {
            throw new DataRetrievalFailureException("All Routing delete requests must have a id.");
        }
    }
}
