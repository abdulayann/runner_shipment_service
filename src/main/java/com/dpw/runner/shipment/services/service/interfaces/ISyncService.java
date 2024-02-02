package com.dpw.runner.shipment.services.service.interfaces;

import org.springframework.http.HttpHeaders;

public interface ISyncService {
    void callSync(String json, String id, String guid, String entity, HttpHeaders headers);
    void pushToKafka(String json, String id, String guid, String entity, String transactionId);
}