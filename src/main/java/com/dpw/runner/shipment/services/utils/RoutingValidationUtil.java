package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;


@Component
public class RoutingValidationUtil {
    @Autowired
    private IShipmentServiceV3 shipmentService;
    @Autowired
    private IConsolidationService consolidationService;
    @Autowired
    private ICustomerBookingService customerBookingService;

    public void validateUpdateRequest(RoutingsRequest request) {
        if (request == null) {
            throw new ValidationException("Update request cannot be null or empty.");
        }
        if (request.getId() == null) {
            throw new ValidationException("Routing ID is missing.");
        }
    }

    public void validateDeleteRequest(CommonGetRequest request) {
        if (request == null) {
            throw new ValidationException("Delete request cannot be null or empty.");
        }
        if (request.getId() == null) {
            throw new ValidationException("Routing ID is missing.");
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
            throw new ValidationException("No Routing found for the ids: " + missingIds);
        }
    }

    public void validateDeleteBulkRequest(List<RoutingsRequest> requests) {
        List<RoutingsRequest> missingIdRequests = requests.stream()
                .filter(req -> req.getId() == null)
                .toList();

        if (!missingIdRequests.isEmpty()) {
            throw new ValidationException("All Routing delete requests must have a id.");
        }
    }

    public void validateModule(RoutingsRequest routingsRequest, String module) {
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            validateShipment(routingsRequest);
        } else if (Constants.CONSOLIDATION.equalsIgnoreCase(module)) {
            validateConsolidation(routingsRequest);
        } else if (Constants.BOOKING.equalsIgnoreCase(module)) {
            validateBooking(routingsRequest);
        }
    }

    private void validateBooking(RoutingsRequest routingsRequest) {
        if (routingsRequest.getBookingId() == null || routingsRequest.getBookingId() <= 0) {
            throw new ValidationException("Booking id is empty");
        }
        Optional<CustomerBooking> customerBooking = customerBookingService.findById(routingsRequest.getBookingId());
        if (customerBooking.isEmpty()) {
            throw new ValidationException("Please provide the valid booking id");
        }
    }

    private void validateConsolidation(RoutingsRequest routingsRequest) {
        if (routingsRequest.getConsolidationId() == null || routingsRequest.getConsolidationId() <= 0) {
            throw new ValidationException("Consolidation id is empty");
        }
        Optional<ConsolidationDetails> consolidationDetails = consolidationService.findById(routingsRequest.getConsolidationId());
        if (consolidationDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid consolidation id");
        }
    }

    private void validateShipment(RoutingsRequest routingsRequest) {
        if (routingsRequest.getShipmentId() == null || routingsRequest.getShipmentId() <= 0) {
            throw new ValidationException("Shipment id is empty");
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentService.findById(routingsRequest.getShipmentId());
        if (shipmentDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid shipment id");
        }
    }

    public void validateRoutingsRequest(List<RoutingsRequest> requests, String module) {
        if (CollectionUtils.isEmpty(requests)) {
            throw new ValidationException("Routings Request list cannot be null or empty.");
        }

        Long commonShipmentId = null;
        Long commonConsolidationId = null;

        for (RoutingsRequest request : requests) {
            Long shipmentId = request.getShipmentId();
            Long consolidationId = request.getConsolidationId();
            validateBothShipmentAndConsolidation(shipmentId, consolidationId);
            if (shipmentId != null) {
                if (commonShipmentId == null) {
                    commonShipmentId = shipmentId;
                } else if (!commonShipmentId.equals(shipmentId)) {
                    throw new ValidationException("All shipmentIds must be the same.");
                }
            }
            if (consolidationId != null) {
                if (commonConsolidationId == null) {
                    commonConsolidationId = consolidationId;
                } else if (!commonConsolidationId.equals(consolidationId)) {
                    throw new ValidationException("All consolidationIds must be the same.");
                }
            }
        }

        validateCommonShipmentAndConsol(commonShipmentId, commonConsolidationId);
        validateModule(requests.get(0), module);
    }

    private void validateCommonShipmentAndConsol(Long commonShipmentId, Long commonConsolidationId) {
        // Final mutual exclusion check
        if (commonShipmentId != null && commonConsolidationId != null) {
            throw new ValidationException("Only one ID type should be used across the entire list: either shipmentId or consolidationId.");
        }
    }

    private void validateBothShipmentAndConsolidation(Long shipmentId, Long consolidationId) {
        if (shipmentId != null && consolidationId != null) {
            throw new ValidationException("Only one ID should be present in each Routings Request: either shipmentId or consolidationId.");
        }
        if (shipmentId == null && consolidationId == null) {
            throw new ValidationException("Either shipmentId or consolidationId must be present.");
        }
    }
    public void validateMainCarriageAdjacencyInIncoming(List<RoutingsRequest> incomingRoutings) {
        boolean inInheritedBlock = false;
        for (RoutingsRequest routing : incomingRoutings) {
            if (routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                    Boolean.TRUE.equals(routing.getInheritedFromConsolidation())) {
                inInheritedBlock = true;
            } else if (inInheritedBlock) {
                // Found a non-inherited routing after inherited block started
                for (RoutingsRequest remaining : incomingRoutings.subList(incomingRoutings.indexOf(routing), incomingRoutings.size())) {
                    if (remaining.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                            Boolean.TRUE.equals(remaining.getInheritedFromConsolidation())) {
                        throw new ValidationException("Invalid routing placement: inherited MAIN_CARRIAGE routing appears non-contiguously. Routing GUID: " + remaining.getGuid());
                    }
                }
                break;
            }
        }
    }

}
