package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
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

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private CommonUtils commonUtils;

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

    public Object validateModule(PackingV3Request packingRequest, String module) {
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            return validateShipment(packingRequest);
        } else if (Constants.CONSOLIDATION.equalsIgnoreCase(module)) {
            return validateConsolidation(packingRequest);
        } else if (Constants.BOOKING.equalsIgnoreCase(module)) {
            return validateBooking(packingRequest);
        }
        return null;
    }

    private CustomerBooking validateBooking(PackingV3Request packingRequest) {
        if (packingRequest.getBookingId() == null || packingRequest.getBookingId() <= 0) {
            throw new ValidationException("Booking id is empty");
        }
        Optional<CustomerBooking> customerBooking = customerBookingService.findById(packingRequest.getBookingId());
        if (customerBooking.isEmpty()) {
            throw new ValidationException("Please provide the valid booking id");
        }
        return customerBooking.get();
    }

    private ConsolidationDetails validateConsolidation(PackingV3Request packingRequest) {
        if (packingRequest.getConsolidationId() == null || packingRequest.getConsolidationId() <= 0) {
            throw new ValidationException("Consolidation id is empty");
        }
        Optional<ConsolidationDetails> consolidationDetails = consolidationService.findById(packingRequest.getConsolidationId());
        if (consolidationDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid consolidation id");
        }
        return consolidationDetails.get();
    }

    private ShipmentDetails validateShipment(PackingV3Request packingRequest) {
        if (packingRequest.getShipmentId() == null || packingRequest.getShipmentId() <= 0) {
            throw new ValidationException("Shipment id is empty");
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentService.findById(packingRequest.getShipmentId());
        if (shipmentDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid shipment id");
        }
        return shipmentDetails.get();
    }

    public void validateSameParentId(List<PackingV3Request> requestList, String moduleType) throws RunnerException {
        if (requestList == null || requestList.isEmpty()) {
            throw new RunnerException("Request can't be null");
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

    public void validateShipmentGateInDate(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getShipmentGateInDate() != null) {
            ConsolidationDetails consolidationDetails = null;
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
            if (!CommonUtils.listIsNullOrEmpty(consoleShipmentMappings)) {
                Long consolidationId = consoleShipmentMappings.get(0).getConsolidationId();
                Optional<ConsolidationDetails> consolidationOptional = consolidationService.findById(consolidationId);
                if (consolidationOptional.isPresent())
                    consolidationDetails = consolidationOptional.get();
            }
            if(consolidationDetails != null && consolidationDetails.getCfsCutOffDate() != null && shipmentDetails.getShipmentGateInDate().isAfter(consolidationDetails.getCfsCutOffDate())) {
                    throw new RunnerException("Shipment Gate In date should not be greater than the CFS Cut Off Date entered at the consolidation level.");
            }
            else if(shipmentDetails.getCarrierDetails().getEtd() != null && shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getCarrierDetails().getEtd()))
                throw new RunnerException("Shipment Gate In Date cannot be greater than ETD.");
        }
    }

    /* In old DG flow (Cargo Security is false), only DG users can update DG shipments
      and DG package cannot be there in non DG Shipment */
    public void validatePackageAfterSave(ShipmentDetails shipmentDetails, List<Packing> packings) {
        if(Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode()) &&
                Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) &&
                !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getCountryAirCargoSecurity())) {

            if(Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && !UserContext.isAirDgUser()) {
                throw new ValidationException("You don't have permission to update DG Shipment");
            }
            if(!Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                for(Packing packing: packings) {
                    if(Boolean.TRUE.equals(packing.getHazardous())) {
                        throw new ValidationException("The shipment contains DG package. Marking the shipment as non DG is not allowed");
                    }
                }
            }

        }
    }
}
