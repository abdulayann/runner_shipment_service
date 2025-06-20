package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;

import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ContainerValidationUtil {

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    /**
     * Validates a bulk update request for containers.
     * <p>
     * Ensures that each {@link ContainerRequest} in the provided list contains a non-null ID. Throws a {@link IllegalArgumentException} if any request item is missing the required
     * identifier.
     * </p>
     *
     * @param requests the list of {@link ContainerRequest} objects to validate
     * @throws IllegalArgumentException if the list is null, empty, or any item has a null ID
     */
    public void validateUpdateBulkRequest(List<ContainerV3Request> requests) {
        if (requests == null || requests.isEmpty()) {
            throw new IllegalArgumentException("Bulk update request cannot be null or empty.");
        }

        for (int index = 0; index < requests.size(); index++) {
            ContainerV3Request container = requests.get(index);
            if (container.getId() == null) {
                throw new IllegalArgumentException(
                        String.format("Container ID is missing for item at index %d. All items must have a valid ID.", index)
                );
            }
        }
    }

    public void validateDeleteBulkRequest(List<ContainerV3Request> requests) {
        if (requests == null || requests.isEmpty()) {
            throw new IllegalArgumentException("Bulk update request cannot be null or empty.");
        }
    }

    public void validateCreateBulkRequest(List<ContainerV3Request> requests) {
        if (requests == null || requests.isEmpty()) {
            throw new IllegalArgumentException("Bulk create request cannot be null or empty.");
        }

        for (int index = 0; index < requests.size(); index++) {
            ContainerV3Request container = requests.get(index);
            if (container.getConsolidationId() == null && container.getShipmentsId() == null) {
                throw new ValidationException("Either ConsolidationId or ShipmentsId must be provided in the request.");
            }
            if(container.getConsolidationId() != null && container.getShipmentsId() != null){
                throw new ValidationException("Only one of ConsolidationId or ShipmentsId should be provided, not both.");
            }
        }
    }

    public void validateContainerNumberUniqueness(String containerNumber, List<Containers> containersList) {
        if (StringUtility.isEmpty(containerNumber) || containerNumber.length() < 10) {
            return;
        }

        boolean exists = containersList.stream()
                .map(Containers::getContainerNumber)
                .filter(Objects::nonNull)
                .anyMatch(existingNumber -> existingNumber.equals(containerNumber));

        if (exists) {
            throw new IllegalArgumentException("Container number '" + containerNumber + "' already exists.");
        }
    }

    public void validateContainerNumberUniquenessForCreateBulk(List<ContainerV3Request> containersList) {
        if (containersList.isEmpty()) {
            return;
        }
        List<String> containerNumbers = containersList.stream()
                .map(ContainerV3Request::getContainerNumber)
                .filter(Objects::nonNull)
                .toList();
        Set<String> uniqueContainerNumbers = new HashSet<>(containerNumbers);
        if (uniqueContainerNumbers.size() != containerNumbers.size()) {
            throw new IllegalArgumentException("Duplicate container numbers found in the request");
        }
    }

    public void validateCanAssignPackageToContainer(ShipmentDetails shipmentDetails) throws RunnerException {
        if (shipmentDetails.getContainerAssignedToShipmentCargo() != null) {
            throw new ValidationException(String.format(
                    "Shipment cargo summary of Shipment - %s already assigned, please detach to assign packages",
                    shipmentDetails.getShipmentId()));
        }
    }

    public void validateBeforeAssignContainer(Map<Long, ShipmentDetails> shipmentDetailsMap) throws RunnerException {
        if(shipmentDetailsMap.values().size() > 1) {
            for(ShipmentDetails shipmentDetails: shipmentDetailsMap.values()) {
                if(Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType())) {
                    throw new ValidationException("Container being or already assigned to FCL Shipment should be linked to only one shipment");
                }
            }
        }
    }


    public void validateOpenForAttachment(List<Containers> containersToDelete) {
        if (containersToDelete == null || containersToDelete.isEmpty()) {
            return;
        }

        Set<Long> consolIds = containersToDelete.stream()
                .map(Containers::getConsolidationId)
                .filter(Objects::nonNull).collect(Collectors.toSet());

        // Check if we have any valid consolidation IDs to process
        if (consolIds.isEmpty()) {
            return;
        }

        List<ConsolidationDetails> consolidationsByIds = consolidationDetailsDao.findConsolidationsByIds(consolIds);

        // Add null check for the returned list and individual objects
        if (ObjectUtils.isNotEmpty(consolidationsByIds) &&
                consolidationsByIds.stream().anyMatch(cd -> cd != null && !Boolean.TRUE.equals(cd.getOpenForAttachment()))) {
            throw new IllegalArgumentException("Changes in cargo is not allowed as Shipment Attachment Allowed value is Off");
        }
    }
}
