package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.HttpHeaders;

import java.util.List;

public interface ISyncService {
    void callSync(String json, String id, String guid, String entity, HttpHeaders headers) throws RunnerException;

    void pushToKafka(String json, String id, String guid, String entity, String transactionId);

    void pushToKafka(String json, String id, String guid, String entity, String transactionId, Integer tenantId, String username, String updatedUsername);

    void callSyncAsync(String json, String id, String guid, String entity, HttpHeaders headers) throws RunnerException;

    void callSyncAsync(List<String> json, List<String> id, List<String> guid, List<String> entity, HttpHeaders headers) throws RunnerException;
}