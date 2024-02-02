package com.dpw.runner.shipment.services.service.interfaces;

import org.springframework.http.HttpHeaders;
import org.springframework.scheduling.annotation.Async;

import java.util.UUID;

public interface ISyncService {
    void callSync(String json, String id, String guid, String entity, HttpHeaders headers);
    @Async
    void pushToKafka(String json, String id, String guid, String entity, String transactionId);
}