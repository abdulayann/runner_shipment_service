package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Slf4j
public class ContainerV3FacadeService {

    @Autowired
    private IContainerV3Service containerV3Service;

    /**
     * Creates or updates containers in bulk based on presence of IDs. If a container request has a null ID, it is treated as a creation request. Otherwise, it is treated as an
     * update.
     *
     * @param containerRequestList List of container requests to be created or updated.
     * @param module               The module from which the request originated (e.g., SHIPMENT, CONSOLIDATION).
     * @return BulkContainerResponse containing the result of all create/update operations.
     */
    @Transactional
    public BulkContainerResponse createUpdateContainer(List<ContainerV3Request> containerRequestList, String module) throws RunnerException {
        BulkContainerResponse finalResponse = new BulkContainerResponse();

        // Separate requests into create and update lists based on whether ID is present
        List<ContainerV3Request> containerRequestForCreate = containerRequestList.stream()
                .filter(request -> ObjectUtils.isEmpty(request.getId())).toList();

        List<ContainerV3Request> containerRequestForUpdate = containerRequestList.stream()
                .filter(request -> ObjectUtils.isNotEmpty(request.getId())).toList();

        if (ObjectUtils.isNotEmpty(containerRequestForCreate)) {
            for (ContainerV3Request containerV3Request : containerRequestForCreate) {
                ContainerResponse containerResponse = containerV3Service.create(containerV3Request, module);
                finalResponse.getContainerResponseList().add(containerResponse);
            }
        }

        if (ObjectUtils.isNotEmpty(containerRequestForUpdate)) {
            BulkContainerResponse bulkContainerResponse = containerV3Service.updateBulk(containerRequestForUpdate, module);
            finalResponse.getContainerResponseList().addAll(bulkContainerResponse.getContainerResponseList());
            finalResponse.setMessage(bulkContainerResponse.getMessage());
        }

        return finalResponse;
    }


}
