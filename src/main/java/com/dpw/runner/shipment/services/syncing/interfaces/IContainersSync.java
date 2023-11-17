package com.dpw.runner.shipment.services.syncing.interfaces;

import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IContainersSync {
    ResponseEntity<?> sync(List<Long> containerIds);
}
