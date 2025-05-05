package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ContainerValidationUtil {

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private IPackingV3Service packingService;


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

        List<Long> containerIds = requests.stream().map(ContainerV3Request::getId).distinct().toList();

        if (requests.size() == 1) {
            List<PackingResponse> packingResponses = packingService.fetchPacksAttachedToContainers(containerIds);
            if (ObjectUtils.isNotEmpty(packingResponses)) {
                PackingResponse packingResponse = packingResponses.get(0);
                throw new IllegalArgumentException(
                        String.format("Selected container is attached with Packs with Shipment Number - %s, Please unassign packs to delete further",
                                packingResponse.getShipmentNumber()));
            }
        } else {
            List<PackingResponse> packingResponses = packingService.fetchPacksAttachedToContainers(containerIds);
            if (ObjectUtils.isNotEmpty(packingResponses)) {
                String details = packingResponses.stream()
                        .map(m -> String.format("Container Number %s - Shipment Number %s", m.getContainerNumber(), m.getShipmentNumber()))
                        .collect(Collectors.joining("\n"));

                String message = "Selected containers cannot be deleted as Containers are attached with following Shipment Packs:\n" +
                        details;

                throw new IllegalArgumentException(message);
            }
        }


    }

    public void validateContainerNumberUniqueness(String containerNumber, List<Containers> containersList){
        if(StringUtility.isEmpty(containerNumber) || containerNumber.length() < 10) return;

        boolean exists = containersList.stream()
             .map(Containers::getContainerNumber)
             .anyMatch(existingNumber -> existingNumber.equals(containerNumber));

        if(exists){
            throw new IllegalArgumentException("Container number '" + containerNumber + "' already exists.");
        }
    }


}
