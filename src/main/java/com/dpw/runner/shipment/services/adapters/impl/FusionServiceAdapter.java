package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IFusionServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CheckCreditBalanceFusionRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;

@Service
@Slf4j
public class FusionServiceAdapter implements IFusionServiceAdapter {
    private final RestTemplate restTemplate;
    private final String baseUrl;
    private final String creditCheckUrlP100;
    @Autowired
    JsonHelper jsonHelper;

    public FusionServiceAdapter(@Qualifier("restTemplateForCreditCheckP100") RestTemplate restTemplate,
                                @Value("${fusion.baseUrl}") String baseUrl,
                                @Value("${fusion.P100.creditBalances}") String creditCheckUrlP100) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
        this.creditCheckUrlP100 = creditCheckUrlP100;
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkCreditLimitP100(CommonRequestModel requestModel) throws RunnerException {
        CheckCreditBalanceFusionRequest request = (CheckCreditBalanceFusionRequest) requestModel.getData();
        String url = baseUrl + creditCheckUrlP100;
        try {
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), Object.class);
            return ResponseHelper.buildDependentServiceResponse(response.getBody(), 0, 0);
        } catch (Exception ex) {
            log.error("Fusion credit check failed due to: {}", jsonHelper.convertToJson(ex.getMessage()));
            throw new RuntimeException("Error from Fusion while fetching credit limit: " + ex.getMessage());
        }
    }
}
