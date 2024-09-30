package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.dto.request.LogHistoryRequest;
import com.dpw.runner.booking.services.dto.response.LogHistoryResponse;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface ILogsHistoryService {
    void createLogHistory(LogHistoryRequest request);
    LogHistoryResponse findByEntityGuidAndTimeStamp(UUID entityGuid, LocalDateTime timeStamp) throws RunnerException;
    List<LogHistoryResponse> findByEntityGuidsAndTimeStamp(List<UUID> entityGuids, LocalDateTime timeStamp) throws RunnerException;
}
