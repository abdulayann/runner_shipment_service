package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IContainersSync {
    ResponseEntity<IRunnerResponse> sync(List<Long> containerIds, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable);
    ResponseEntity<IRunnerResponse> sync(List<Long> containerIds, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable, String transactionId);
}
