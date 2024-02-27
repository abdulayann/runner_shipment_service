package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.reportService.MailAuditLogRequest;
import org.junit.runner.Runner;
import org.springframework.http.ResponseEntity;

import java.util.Map;

public interface IReportService {
    ResponseEntity<?> postRequest(MailAuditLogRequest body, Map<String, String> parameters) throws Exception;
}
