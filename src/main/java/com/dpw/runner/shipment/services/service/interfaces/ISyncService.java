package com.dpw.runner.shipment.services.service.interfaces;

import org.springframework.scheduling.annotation.Async;

import java.util.UUID;

public interface ISyncService {
    @Async
    void callSync(String json, Long id, UUID guid, String entity);
}