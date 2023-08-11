package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;

public interface IAuditLogService {
    Resource downloadExcel(CommonRequestModel commonRequestModel);
    ResponseEntity<?> list(CommonRequestModel commonRequestModel);
    void addAuditLog(AuditLogMetaData auditLogMetaData) throws IllegalAccessException, NoSuchFieldException, JsonProcessingException;
}
