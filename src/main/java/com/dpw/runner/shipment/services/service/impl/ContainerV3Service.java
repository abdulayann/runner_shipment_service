package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ContainerV3Service implements IContainerV3Service {

    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public ContainerResponse create(ContainerRequest containerRequest) {
        Containers container = jsonHelper.convertValue(containerRequest, Containers.class);
        Containers savedContainer = containerRepository.save(container);
        return jsonHelper.convertValue(savedContainer, ContainerResponse.class);
    }
    
}
