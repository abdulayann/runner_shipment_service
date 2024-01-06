package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IContainersSync {
    ResponseEntity<?> sync(List<Long> containerIds, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable);
}
