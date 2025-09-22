package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ContainerV3PatchBulkUpdateRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.mapper.ContainerV3Mapper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;

import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;

@Service
@Slf4j
public class ContainerV3FacadeService {

    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ContainerV3Mapper containerV3Mapper;

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
        validateContainerNumberFormat(containerRequestList);
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

    public void validateContainerNumberFormat(List<ContainerV3Request> containerRequestList) {
        if (!CollectionUtils.isEmpty(containerRequestList)) {
            containerRequestList.forEach(containerV3Request -> {
                if(StringUtility.isNotEmpty(containerV3Request.getContainerNumber())) {
                    ContainerNumberCheckResponse containerNumberCheckResponse = containerV3Service.validateContainerNumber(containerV3Request.getContainerNumber().trim());
                    if (containerNumberCheckResponse != null && !containerNumberCheckResponse.isSuccess()
                            && (containerNumberCheckResponse.getIsLastDigitCorrect() == null || containerNumberCheckResponse.getIsLastDigitCorrect())
                    ) {
                        throw new ValidationException("Invalid container number format");
                    }
                    if(containerNumberCheckResponse != null && containerNumberCheckResponse.getLastDigit() != null) {
                        containerV3Request.setContainerNumber(containerV3Request.getContainerNumber().trim() + containerNumberCheckResponse.getLastDigit());
                    }
                }
            });
        }
    }

    public BulkContainerResponse updatePatchContainer(ContainerV3PatchBulkUpdateRequest request, String module) throws RunnerException {
        if(listIsNullOrEmpty(request.getContainerIds())) {
            throw new ValidationException("No container Ids provided");
        }
        List<Containers> containers = containerDao.findByIdIn(request.getContainerIds());
        if(listIsNullOrEmpty(containers)) {
            throw new ValidationException("No containers found for the given IDs");
        }
        Long consolidationId = containers.get(0).getConsolidationId();
        boolean allMatch = containers.stream()
                .allMatch(c -> Objects.equals(c.getConsolidationId(), consolidationId));
        if (!allMatch) {
            throw new ValidationException("All containers must have the same consolidationId");
        }
        List<ContainerV3Request> containerV3Requests = jsonHelper.convertValueToList(containers, ContainerV3Request.class);

        for(ContainerV3Request containerV3Request : containerV3Requests) {
            containerV3Mapper.update(request.getContainerV3PatchRequest(), containerV3Request);
        }

        return containerV3Service.updateBulk(containerV3Requests, module);
    }

}
