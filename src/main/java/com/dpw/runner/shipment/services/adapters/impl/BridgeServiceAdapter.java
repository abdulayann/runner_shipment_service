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
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;

import java.util.List;

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
        String authToken = generateToken(bridgeServiceConfig.getUserName(), bridgeServiceConfig.getPassword(), bridgeServiceConfig.getTenantCode());
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

    private String generateToken(String userName, String password, String tenantCode) throws RunnerException {
        AuthLoginRequest authLoginRequest = AuthLoginRequest.builder()
            .tenantCode(tenantCode)
            .username(userName)
            .password(password)
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

    @Override
    public IRunnerResponse requestOutBoundFileTransfer(CommonRequestModel commonRequestModel) throws RunnerException {
        String authToken = generateToken(bridgeServiceConfig.getOutBoundTransferUserName(), bridgeServiceConfig.getOutBoundTransferPassword(), null);
        HttpHeaders headers = new HttpHeaders();
        headers.add(HttpHeaders.AUTHORIZATION, "Bearer " + authToken);
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setAccept(List.of(MediaType.APPLICATION_JSON));
        var url = bridgeServiceConfig.getBaseUrl() + bridgeServiceConfig.getRequestUrl();

        BridgeRequest request  = (BridgeRequest) commonRequestModel.getData();

        HttpEntity<String> httpEntity = new HttpEntity<>(jsonHelper.convertToJson(request), headers);

        try {
            var bridgeResponse = restTemplate.postForEntity(url, httpEntity, BridgeServiceResponse.class);
            log.info("Received data from bridge service for file transfer outbound integration: {}", jsonHelper.convertToJson(bridgeResponse));
            return bridgeResponse.getBody();
        }
        catch (Exception e) {
            log.error("Error while hitting bridge service request endpoint", e);
            String errorBody = (e instanceof HttpStatusCodeException)
                    ? ((HttpStatusCodeException) e).getResponseBodyAsString()
                    : e.getMessage();

            throw new RunnerException(
                    (request.getRequestCode().equalsIgnoreCase("GBLCS") || request.getRequestCode().equalsIgnoreCase("EWLAIREXP"))
                            ? extractErrorDescription(errorBody)
                            : errorBody
            );
        }

    }

    private String extractErrorDescription(String detailMessage) {
        try {
            // Find first JSON starting point
            int jsonStartIndex = detailMessage.indexOf("{");
            if (jsonStartIndex == -1) {
                return "Invalid message format";
            }

            // Extract only the JSON substring
            String json = detailMessage.substring(jsonStartIndex).trim();

            ObjectMapper mapper = new ObjectMapper();
            JsonNode rootNode = mapper.readTree(json);

            // Navigate to error.description
            JsonNode errorNode = rootNode.path("error");
            if (errorNode.has("description")) {
                String rawDescription = errorNode.get("description").asText();

                // Remove leading/trailing square brackets if present
                if (rawDescription.startsWith("[") && rawDescription.endsWith("]")) {
                    rawDescription = rawDescription.substring(1, rawDescription.length() - 1).trim();
                }
                return rawDescription;
            }

            return "Description not found";
        } catch (Exception e) {
            return "Error parsing detailMessage: " + e.getMessage();
        }
    }
}
