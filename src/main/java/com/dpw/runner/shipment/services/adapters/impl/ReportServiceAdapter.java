package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IReportService;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.reportService.MailAuditLogRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.client.utils.URIBuilder;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;

@Service
@Slf4j
public class ReportServiceAdapter implements IReportService {

    private final RestTemplate restTemplate;
    private final String baseUrl;

    public ReportServiceAdapter(@Qualifier("restTemplateForReportService") RestTemplate restTemplate,
                                @Value("${reportservicesettings.emailauditcreate}") String baseUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
    }

    private static String getQueryString(Map<String, String> parameters) {
        try {
            URIBuilder uriBuilder = new URIBuilder();

            if (parameters != null && !parameters.isEmpty()) {
                for (Map.Entry<String, String> entry : parameters.entrySet()) {
                    uriBuilder.setParameter(entry.getKey(), entry.getValue());
                }
            }

            URI uri = uriBuilder.build();
            return uri.getRawQuery();

        } catch (URISyntaxException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> postRequest(MailAuditLogRequest body, Map<String, String> parameters) throws RunnerException, URISyntaxException {
        URI uri = new URI(baseUrl);
        if (parameters != null && !parameters.isEmpty()) {
            uri = new URI(uri.toString() + "?" + getQueryString(parameters));
        }

        log.info("Calling Report Service uri {} : with request: {}", uri.toString(), body.toString());
        ResponseEntity<?> responseEntity;
        try {
            responseEntity = restTemplate.exchange(RequestEntity.post(uri).body(body), Object.class);
        } catch (HttpClientErrorException ex) {
            log.error("Report Service call failed : with exception : " + ex.getMessage());
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
        log.info("Retrieve CRP: with response: {}", responseEntity);
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(), 0, 0);
    }
}
