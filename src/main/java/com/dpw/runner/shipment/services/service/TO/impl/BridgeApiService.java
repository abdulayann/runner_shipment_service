package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.commons.EAWBConstants;
import com.dpw.runner.shipment.services.exception.exceptions.BridgeServiceException;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import com.dpw.runner.shipment.services.service.TO.request.BridgeLoginRequest;
import com.dpw.runner.shipment.services.service.TO.request.BridgeValidateRequest;
import com.dpw.runner.shipment.services.service.TO.response.BridgeLoginResponse;
import com.dpw.runner.shipment.services.utils.AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;

import java.util.Objects;
import java.util.UUID;

@Service
@Slf4j
@EnableRetry
public class BridgeApiService {

    @Value("${bridge.baseUrl}")
    private String BRIDGE_BASE_URL;

    @Value("${bridge.login}")
    private String BRIDGE_LOGIN_URL;

    @Value("${bridge.validate}")
    private String BRIDGE_VALIDATE_URL;

    @Value("${bridge.tenantCode}")
    private String BRIDGE_TENANT;

    @Value("${bridge.username}")
    private String BRIDGE_USERNAME;

    @Value("${bridge.password}")
    private String BRIDGE_PASSWORD;

    @Value("${bridge.xClientType}")
    private String BRIDGE_X_CLIENT;

    @Autowired
    private RestTemplate restTemplate;

    private String LOGIN_TOKEN;

    public void loginAndSetAuthToken() {
        try {
            BridgeLoginRequest loginRequest = BridgeLoginRequest.builder()
                    .tenantCode(BRIDGE_TENANT)
                    .username(BRIDGE_USERNAME)
                    .password(BRIDGE_PASSWORD)
                    .build();
            HttpEntity<BridgeLoginRequest> entity = new HttpEntity<>(loginRequest, AuthHelper.getBridgeServiceHeaders(BRIDGE_X_CLIENT));
            ResponseEntity<BridgeLoginResponse> loginResponse = restTemplate.postForEntity( BRIDGE_BASE_URL + BRIDGE_LOGIN_URL, entity, BridgeLoginResponse.class);
            if (loginResponse.getStatusCode() != HttpStatus.OK || Objects.isNull(loginResponse.getBody())
                    || Objects.isNull(loginResponse.getBody().getAccessToken()))
                throw new UnAuthorizedException("Failed to fetch token from Bridge Service!");
            LOGIN_TOKEN = loginResponse.getBody().getAccessToken();
        } catch (Exception er) {
            log.error("Error While Login to bridge {}", er.getMessage());
            throw new UnAuthorizedException(er.toString());
        }
    }

    @Retryable(retryFor = UnAuthorizedException.class, maxAttempts = 3)
    public String validateTemplate(String template, String payload) {
        BridgeValidateRequest bridgeValidateRequest = BridgeValidateRequest
                .builder()
                .templateType(EAWBConstants.templateType)
                .templateId(UUID.randomUUID().toString())
                .template(template)
                .payload(payload)
                .build();
        if (Objects.isNull(LOGIN_TOKEN)) {
            loginAndSetAuthToken();
        }

        HttpEntity<BridgeValidateRequest> entity = new HttpEntity<>(bridgeValidateRequest, AuthHelper.getBridgeServiceTokenHeader(BRIDGE_X_CLIENT, LOGIN_TOKEN));

        try {
            ResponseEntity<String> response = restTemplate.postForEntity(BRIDGE_BASE_URL + BRIDGE_VALIDATE_URL, entity, String.class);
            return response.getBody();
        } catch (HttpStatusCodeException exception) {
            if (exception.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                log.error("UnAuthorizedException Exception received from Bridge service: {}", exception.getMessage());
                loginAndSetAuthToken();
                throw new UnAuthorizedException("UnAuthorizedException Exception received from Bridge service");
            }
            log.error("Exception received from Bridge service: {}", exception.getMessage());
            throw new BridgeServiceException(exception.getMessage());
        } catch (Exception exception){
            log.error("Exception received from Bridge service: {}", exception.getMessage());
            throw new BridgeServiceException(exception.getMessage());
        }
    }
}
