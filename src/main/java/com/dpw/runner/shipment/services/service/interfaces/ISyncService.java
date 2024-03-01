package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.HttpHeaders;

public interface ISyncService {
    void callSync(String json, String id, String guid, String entity, HttpHeaders headers) throws RunnerException;
    void pushToKafka(String json, String id, String guid, String entity, String transactionId);
    void callSyncAsync(String json, String id, String guid, String entity, HttpHeaders headers) throws RunnerException;
}