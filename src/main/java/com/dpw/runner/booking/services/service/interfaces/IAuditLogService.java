package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.booking.services.commons.requests.CommonRequestModel;
import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;

import java.lang.reflect.InvocationTargetException;

public interface IAuditLogService {
    Resource downloadExcel(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    void addAuditLog(AuditLogMetaData auditLogMetaData) throws IllegalAccessException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, NoSuchMethodException, RunnerException;
}
