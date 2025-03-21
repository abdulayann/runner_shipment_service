package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BridgeServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBridgeServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.bridgeService.AuthLoginRequest;
import com.dpw.runner.shipment.services.dto.request.bridgeService.BridgeRequest;
import com.dpw.runner.shipment.services.dto.request.bridgeService.TactBridgePayload;
import com.dpw.runner.shipment.services.dto.response.bridgeService.AuthLoginResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.RequestEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@Slf4j
public class BridgeServiceAdapter implements IBridgeServiceAdapter {

    private RestTemplate restTemplate;
    private BridgeServiceConfig bridgeServiceConfig;
    private JsonHelper jsonHelper;

    @Autowired
    public BridgeServiceAdapter(@Qualifier("restTemplateForBridgeService") RestTemplate restTemplate, BridgeServiceConfig bridgeServiceConfig, JsonHelper jsonHelper) {
        this.restTemplate = restTemplate;
        this.bridgeServiceConfig = bridgeServiceConfig;
        this.jsonHelper = jsonHelper;
    }


    @Override
    public IRunnerResponse requestTactResponse(CommonRequestModel commonRequestModel) throws RunnerException {
        String authToken = generateToken();
        HttpHeaders headers = new HttpHeaders();
        headers.add(HttpHeaders.AUTHORIZATION, "Bearer " + authToken);
        var url = bridgeServiceConfig.getBaseUrl() + bridgeServiceConfig.getRequestUrl();

        TactBridgePayload tactBridgePayload = (TactBridgePayload) commonRequestModel.getData();
        BridgeRequest request = BridgeRequest.builder().requestCode(bridgeServiceConfig.getTactIntegrationRequestCode())
            .payload(tactBridgePayload).build();

        HttpEntity<String> httpEntity = new HttpEntity<>(jsonHelper.convertToJson(request), headers);

        try {
            var bridgeResponse = restTemplate.postForEntity(url, httpEntity, BridgeServiceResponse.class);
            log.info("Received data from bridge service for tact integration: " + jsonHelper.convertToJson(bridgeResponse));
            return bridgeResponse.getBody();
        }
        catch (Exception e) {
            log.error("Error while hitting bridge service request endpoint", e);
            throw new RunnerException(e.getMessage());
        }

    }

    private String generateToken() throws RunnerException {
        AuthLoginRequest authLoginRequest = AuthLoginRequest.builder()
            .tenantCode(bridgeServiceConfig.getTenantCode())
            .username(bridgeServiceConfig.getUserName())
            .password(bridgeServiceConfig.getPassword())
            .build();
        var url = bridgeServiceConfig.getBaseUrl() + bridgeServiceConfig.getAuthLoginUrl();
        try {
            var response = restTemplate.exchange(RequestEntity.post(url).body(jsonHelper.convertToJson(authLoginRequest)), AuthLoginResponse.class);
            log.info("Received token from bridge service");
            return response.getBody().getAccessToken();
        }
        catch (Exception e) {
            log.error("Error while generating token for bridge service", e);
            throw new RunnerException(e.getMessage());
        }
    }
}
