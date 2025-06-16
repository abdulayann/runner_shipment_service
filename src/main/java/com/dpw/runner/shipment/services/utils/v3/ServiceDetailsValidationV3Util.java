package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
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
public class ServiceDetailsValidationV3Util {
    @Autowired
    private IShipmentServiceV3 shipmentService;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    public void validateUpdateBulkRequest(List<ServiceDetailsRequest> requests, List<ServiceDetails> existingServices) {
        List<Long> incomingIds = requests.stream()
                .map(ServiceDetailsRequest::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        Set<Long> existingIds = existingServices.stream()
                .map(ServiceDetails::getId)
                .collect(Collectors.toSet());

        // Validate: If any ID in the request is not found in DB, throw exception
        List<Long> missingIds = incomingIds.stream()
                .filter(id -> !existingIds.contains(id))
                .toList();

        if (!missingIds.isEmpty()) {
            throw new DataRetrievalFailureException("No service details found for the ids: " + missingIds);
        }
        if (CommonUtils.listIsNullOrEmpty(requests))
            return;
        requests.forEach(serviceDetailsRequest -> {
            if (CommonUtils.isStringNullOrEmpty(serviceDetailsRequest.getServiceType())) {
                throw new ValidationException("All service details requests must have service type.");
            }
        });
    }

    public void validateDeleteBulkRequest(List<ServiceDetailsRequest> requests) {
        List<ServiceDetailsRequest> missingIdRequests = requests.stream()
                .filter(req -> req.getId() == null)
                .toList();

        if (!missingIdRequests.isEmpty()) {
            throw new ValidationException("All service details delete requests must have a id.");
        }
    }

    public void validateModule(ServiceDetailsRequest serviceDetailsRequest, String module) {
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            validateShipment(serviceDetailsRequest);
        } else if (Constants.CONSOLIDATION.equalsIgnoreCase(module)) {
            validateConsolidation(serviceDetailsRequest);
        }
    }

    private void validateConsolidation(ServiceDetailsRequest serviceDetailsRequest) {
        if (serviceDetailsRequest.getConsolidationId() == null || serviceDetailsRequest.getConsolidationId() <= 0) {
            throw new ValidationException("Consolidation id is empty");
        }
        Optional<ConsolidationDetails> consolidationDetails = consolidationService.findById(serviceDetailsRequest.getConsolidationId());
        if (consolidationDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid consolidation id");
        }
    }

    private void validateShipment(ServiceDetailsRequest serviceDetailsRequest) {
        if (serviceDetailsRequest.getShipmentId() == null || serviceDetailsRequest.getShipmentId() <= 0) {
            throw new ValidationException("Shipment id is empty");
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentService.findById(serviceDetailsRequest.getShipmentId());
        if (shipmentDetails.isEmpty()) {
            throw new ValidationException("Please provide the valid shipment id");
        }
    }

    public void validateSameParentId(List<ServiceDetailsRequest> requestList, String moduleType) throws RunnerException {
        if (requestList == null || requestList.isEmpty()) {
            throw new RunnerException("Request can't be null");
        }

        switch (moduleType.toUpperCase()) {
            case Constants.SHIPMENT -> validateSameId(requestList, ServiceDetailsRequest::getShipmentId, "shipmentId");
            case Constants.CONSOLIDATION -> validateSameId(requestList, ServiceDetailsRequest::getConsolidationId, "consolidationId");
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        }
    }

    @SuppressWarnings("java:S4276")
    private void validateSameId(List<ServiceDetailsRequest> list,
                                Function<ServiceDetailsRequest, Long> idExtractor,
                                String fieldName) {
        Long expectedId = null;

        for (ServiceDetailsRequest request : list) {
            Long currentId = idExtractor.apply(request);
            if (currentId == null) {
                throw new IllegalArgumentException("All service details requests must have the " + fieldName + ".");
            }
            if (expectedId == null) {
                expectedId = currentId;
            } else if (!expectedId.equals(currentId)) {
                throw new IllegalArgumentException("All service details requests must have the same " + fieldName + ".");
            }
        }
    }
}
