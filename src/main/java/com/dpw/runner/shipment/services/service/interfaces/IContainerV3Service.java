package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;

public interface IContainerV3Service {
    ContainerResponse create(ContainerRequest containerRequest);
    BulkContainerResponse updateBulk(List<ContainerRequest> request);
    BulkContainerResponse deleteBulk(List<ContainerRequest> request);
    ContainerSummaryResponse calculateContainerSummary(List<Containers> containersList, String transportMode, String containerCategory) throws RunnerException;
    BulkContainerResponse getContainers(ListCommonRequest listCommonRequest);
}
