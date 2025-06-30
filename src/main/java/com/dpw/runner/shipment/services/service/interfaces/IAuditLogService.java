package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;

import java.lang.reflect.InvocationTargetException;

public interface IAuditLogService {
    Resource downloadExcel(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, String xSource);

    void addAuditLog(AuditLogMetaData auditLogMetaData) throws IllegalAccessException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, NoSuchMethodException, RunnerException;
}
