package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.dto.request.reportService.MailAuditLogRequest;
import org.springframework.http.ResponseEntity;

import java.util.Map;

public interface IReportService {
    ResponseEntity<?> postRequest(MailAuditLogRequest body, Map<String, String> parameters) throws Exception;
}
