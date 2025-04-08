package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import java.util.List;

public interface IContainerV3Service {
    ContainerResponse create(ContainerRequest containerRequest);
    BulkContainerResponse updateBulk(List<ContainerRequest> request);
    BulkContainerResponse deleteBulk(List<ContainerRequest> request);
}
