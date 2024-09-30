package com.dpw.runner.booking.services.adapters.interfaces;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.dpw.runner.booking.services.dto.request.reportService.MailAuditLogRequest;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.net.URISyntaxException;
import java.util.Map;

public interface IReportService {
    ResponseEntity<IRunnerResponse> postRequest(MailAuditLogRequest body, Map<String, String> parameters) throws RunnerException, URISyntaxException;
}
