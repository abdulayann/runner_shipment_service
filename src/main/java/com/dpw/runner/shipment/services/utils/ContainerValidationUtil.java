package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
@Component
public class ContainerValidationUtil {

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private IPackingV3Service packingService;

    @Autowired
    private IShipmentServiceV3 shipmentService;


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

        if (requests.stream().anyMatch(cr -> !Boolean.TRUE.equals(cr.getOpenForAttachment()))) {
            throw new IllegalArgumentException("Changes in cargo is not allowed as Shipment Attachment Allowed value is Off");
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

    public void validateCanAssignPackageToContainer(ShipmentDetails shipmentDetails) throws RunnerException {
        if (shipmentDetails.getContainerAssignedToShipmentCargo() != null) {
            throw new RunnerException(String.format(
                    "Shipment cargo summary of Shipment - %s already assigned, please detach to assign packages",
                    shipmentDetails.getShipmentId()));
        }
    }

    public void validateBeforeAssignContainer(Map<Long, ShipmentDetails> shipmentDetailsMap) throws RunnerException {
        if(shipmentDetailsMap.values().size() > 1) {
            for(ShipmentDetails shipmentDetails: shipmentDetailsMap.values()) {
                if(Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType())) {
                    throw new RunnerException("Container being or already assigned to FCL Shipment should be linked to only one shipment");
                }
            }
        }
    }


}
