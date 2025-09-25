package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingLegWarning;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;


@Component
@AllArgsConstructor
public class RoutingValidationUtil {

    private final IShipmentServiceV3 shipmentService;

    private final IConsolidationService consolidationService;

    private final ICustomerBookingService customerBookingService;

    private final IRoutingsDao routingsV3Dao;


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

    public void validateMainCarriageRoutingLegs(List<RoutingsRequest> routingsList) {
        RoutingsRequest firstMainCarriageRoutingLeg = findMainCarriageLeg(routingsList, true);
        RoutingsRequest lastMainCarriageRoutingLeg = findMainCarriageLeg(routingsList, false);
        if (Objects.isNull(firstMainCarriageRoutingLeg) || Objects.isNull(lastMainCarriageRoutingLeg)) return;

        validateMainCarriageRoutingLegs(firstMainCarriageRoutingLeg.getEtd(), lastMainCarriageRoutingLeg.getEta(),
                firstMainCarriageRoutingLeg.getAtd(), lastMainCarriageRoutingLeg.getAta());
    }

    private void validateMainCarriageRoutingLegs(LocalDateTime etd, LocalDateTime eta, LocalDateTime atd, LocalDateTime ata) {

        Set<String> validationErrors = new LinkedHashSet<>();
        if (Objects.nonNull(etd) && Objects.nonNull(eta) && etd.isAfter(eta.plusHours(24))) {
            validationErrors.add("ETA (Last main-carriage) cannot be less than ETD (First Main-carriage)");
        }

        if (Objects.nonNull(atd) && Objects.nonNull(ata) && ata.isBefore(atd.minusHours(24))) {
            validationErrors.add("ATA (Last Main-carriage) cannot be less than ATD (First Main-carriage)");
        }

        if (Objects.nonNull(ata) && ata.isAfter(LocalDateTime.now().plusHours(24))) {
            validationErrors.add("ATA (Last Main-carriage) cannot be more than Current Date");
        }

        if (Objects.nonNull(atd) && atd.isAfter(LocalDateTime.now().plusHours(24))) {
            validationErrors.add("ATD (First Main-carriage) cannot be more than Current Date");
        }

        if (!validationErrors.isEmpty()) {
            throw new ValidationException(String.join("###", validationErrors));
        }
    }

    private RoutingsRequest findMainCarriageLeg(List<RoutingsRequest> routingsList, boolean toFindFirstMainCarriageRoutingLeg) {

        RoutingsRequest currMainCarriageRoutingLeg = null;
        for (RoutingsRequest routingsRequest : routingsList) {
            if (RoutingCarriage.MAIN_CARRIAGE.equals(routingsRequest.getCarriage())) {
                if (toFindFirstMainCarriageRoutingLeg) return routingsRequest;
                else currMainCarriageRoutingLeg = routingsRequest;
            }
        }
        return currMainCarriageRoutingLeg;
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

    public void checkIfMainCarriageAllowed(RoutingsRequest routingsRequest) {
        if (routingsRequest.getId() == null && routingsRequest.getCarriage() == RoutingCarriage.MAIN_CARRIAGE) {
            Optional<ShipmentDetails> shipmentDetails = shipmentService.findById(routingsRequest.getShipmentId());
            if (shipmentDetails.isEmpty()) {
                throw new ValidationException("Please provide the valid shipment id");
            }
            if (StringUtility.isNotEmpty(shipmentDetails.get().getConsolRef())) {
                int inheritCarriage = routingsV3Dao.findByShipmentId(routingsRequest.getShipmentId()).stream().filter(Routings::getInheritedFromConsolidation).toList().size();
                if (inheritCarriage == 0) {
                    throw new ValidationException("Adding a Main Carriage can not be allowed if attached console does not have Main Carriage");
                }
            }
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

    public void validateBulkUpdateRoutingRequest(BulkUpdateRoutingsRequest request, String module) {
        if (CollectionUtils.isEmpty(request.getRoutings()) && (request.getEntityId() == null || request.getEntityId() <= 0)) {
            throw new ValidationException("Routing list or entity id is empty");
        } else if (!CollectionUtils.isEmpty(request.getRoutings())) {
            //validate if list has same shipment id in all and shipment id and consol id should not be present at a time
            validateRoutingsRequest(request.getRoutings(), module);
            validateMainCarriageAdjacencyInIncoming(request.getRoutings());
        }
    }

    /**
     * Validates routing legs for ETD/ETA timing conflicts
     *
     * @param routingsResponses List of routing requests sorted by leg number
     * @param legsWarning
     * @return List of validation error messages
     */
    public List<String> validateRoutingLegs(List<RoutingsResponse> routingsResponses, Map<String, RoutingLegWarning> legsWarning) {
        List<String> validationErrors = new ArrayList<>();

        if (routingsResponses == null || routingsResponses.size() <= 1) {
            return validationErrors; // No validation needed for null, empty, or single leg
        }

        // Validate each leg against the previous one (starting from second leg)
        for (int i = 1; i < routingsResponses.size(); i++) {
            RoutingsResponse currentLeg = routingsResponses.get(i);
            RoutingsResponse previousLeg = routingsResponses.get(i - 1);

            String errorMessage = validateLegTiming(currentLeg, previousLeg);
            if (errorMessage != null) {
                validationErrors.add(errorMessage);
                RoutingLegWarning routingLegWarning = new RoutingLegWarning();
                routingLegWarning.setEtd(errorMessage);
                legsWarning.put(currentLeg.getLeg() + Constants.EMPTY_STRING, routingLegWarning);
                setPreviousLegData(legsWarning, previousLeg, currentLeg);

            }
        }

        return validationErrors;
    }

    private static void setPreviousLegData(Map<String, RoutingLegWarning> legsWarning, RoutingsResponse previousLeg, RoutingsResponse currentLeg) {
        RoutingLegWarning routingLegWarning = legsWarning.get(previousLeg.getLeg() + Constants.EMPTY_STRING);
        if (Objects.isNull(routingLegWarning)) {
            routingLegWarning = new RoutingLegWarning();
        }
        String etaWarning = String.format("ETA (of Leg No. %d) should be lesser than ETD (of Leg No. %d)",
                previousLeg.getLeg(), currentLeg.getLeg());
        routingLegWarning.setEta(etaWarning);
        legsWarning.put(previousLeg.getLeg() + Constants.EMPTY_STRING, routingLegWarning);
    }

    /**
     * Validates timing between current leg and previous leg
     *
     * @param currentLeg  Current routing leg
     * @param previousLeg Previous routing leg
     * @return Error message if validation fails, null if validation passes
     */
    private static String validateLegTiming(RoutingsResponse currentLeg, RoutingsResponse previousLeg) {
        // Check if required fields are present
        if (currentLeg.getEtd() == null || previousLeg.getEta() == null) {
            return null; // Skip validation if required dates are missing
        }

        LocalDateTime currentEtd = currentLeg.getEtd();
        LocalDateTime previousEta = previousLeg.getEta();

        // Check if current ETD is less than (previous ETA)
        if (currentEtd.isBefore(previousEta)) {
            return String.format("ETD (of Leg No. %d) should be greater than ETA (of Leg No. %d)",
                    currentLeg.getLeg(), previousLeg.getLeg());
        }

        return null; // Validation passed
    }

    /**
     * Alternative method that returns a single formatted message
     *
     * @param routingsResponses List of routing requests
     * @param legsWarning
     * @return Single string with all validation errors, or null if no errors
     */
    public String getWarningMessage(List<RoutingsResponse> routingsResponses, Map<String, RoutingLegWarning> legsWarning) {
        List<String> errors = validateRoutingLegs(routingsResponses, legsWarning);

        if (errors.isEmpty()) {
            return null;
        }

        return String.join("###", errors);
    }

    /**
     * Added validation for routing voyage.
     * In every scenario its max length must not cross 20  digit.
     * If it is then throw Validation error.
     * @param request Update Request
     */
    public void validateVoyageLengthRequest(BulkUpdateRoutingsRequest request) {
        if (Objects.nonNull(request) && Objects.nonNull(request.getRoutings())) {
            for (RoutingsRequest routingsRequest : request.getRoutings()) {
                if (Objects.nonNull(routingsRequest.getMode())
                        && TRANSPORT_MODE_SEA.equalsIgnoreCase(routingsRequest.getMode())
                        && StringUtility.isNotEmpty(routingsRequest.getVoyage())
                        && routingsRequest.getVoyage().length() > 20) {
                    throw new ValidationException("max size is 20 for voyage");
                }
            }
        }
    }
}
