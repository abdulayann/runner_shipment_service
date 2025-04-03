package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;

public interface IContainerV3Service {
    ContainerResponse create(ContainerRequest containerRequest);
}
