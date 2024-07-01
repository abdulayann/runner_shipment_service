package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.UUID;

public interface ILogsHistoryService {
    void createLogHistory(LogHistoryRequest request);
    LogHistoryResponse retrieveByEntityGuid(UUID entityGuid) throws RunnerException;
}
