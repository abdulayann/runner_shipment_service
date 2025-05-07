package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Component
public class PackingValidationV3Util {
    @Autowired
    private IShipmentServiceV3 shipmentService;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private ICustomerBookingService customerBookingService;

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

    public void validateModule(PackingV3Request packingRequest, String module) {
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            validateShipment(packingRequest);
        } else if (Constants.CONSOLIDATION.equalsIgnoreCase(module)) {
            validateConsolidation(packingRequest);
        } else if (Constants.BOOKING.equalsIgnoreCase(module)) {
            validateBooking(packingRequest);
        }
    }

    private void validateBooking(PackingV3Request packingRequest) {
        if (packingRequest.getBookingId() == null || packingRequest.getBookingId() <= 0) {
            throw new ValidationException("Booking id is empty");
        }
        Optional<CustomerBooking> customerBooking = customerBookingService.findById(packingRequest.getBookingId());
        if (customerBooking.isEmpty()) {
            throw new ValidationException("Please provide the valid booking id");
        }
    }

    private void validateConsolidation(PackingV3Request packingRequest) {
        if (packingRequest.getConsolidationId() == null || packingRequest.getConsolidationId() <= 0) {
            throw new ValidationException("Consolidation id is empty");
        }
        Optional<ConsolidationDetails> consolidationDetails = consolidationService.findById(packingRequest.getConsolidationId());
        if (consolidationDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid consolidation id");
        }
    }

    private void validateShipment(PackingV3Request packingRequest) {
        if (packingRequest.getShipmentId() == null || packingRequest.getShipmentId() <= 0) {
            throw new ValidationException("Shipment id is empty");
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentService.findById(packingRequest.getShipmentId());
        if (shipmentDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid shipment id");
        }
    }

    public void validateSameParentId(List<PackingV3Request> requestList, String moduleType) {
        if (requestList == null || requestList.isEmpty()) {
            return;
        }

        switch (moduleType.toUpperCase()) {
            case Constants.SHIPMENT -> validateSameId(requestList, PackingV3Request::getShipmentId, "shipmentId");
            case Constants.CONSOLIDATION -> validateSameId(requestList, PackingV3Request::getConsolidationId, "consolidationId");
            case Constants.BOOKING -> validateSameId(requestList, PackingV3Request::getBookingId, "bookingId");
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        }
    }

    @SuppressWarnings("java:S4276")
    private void validateSameId(List<PackingV3Request> list,
                                Function<PackingV3Request, Long> idExtractor,
                                String fieldName) {
        Long expectedId = null;

        for (PackingV3Request request : list) {
            Long currentId = idExtractor.apply(request);
            if (currentId == null) {
                throw new IllegalArgumentException("All packing requests must have the " + fieldName + ".");
            }
            if (expectedId == null) {
                expectedId = currentId;
            } else if (!expectedId.equals(currentId)) {
                throw new IllegalArgumentException("All packing requests must have the same " + fieldName + ".");
            }
        }
    }
}
