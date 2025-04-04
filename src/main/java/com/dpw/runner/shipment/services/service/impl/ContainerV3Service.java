package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ContainerV3Service implements IContainerV3Service {

    @Override
    public ContainerResponse create(ContainerRequest containerRequest) {
        return null;
    }
    
}
