package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformCreateRequest;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformUpdateRequest;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;

@Service
@Slf4j
public class PlatformServiceAdapter implements IPlatformServiceAdapter {
    private final RestTemplate restTemplate;
    private final String baseUrl;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    public PlatformServiceAdapter(@Qualifier("restTemplateForPlatform") RestTemplate restTemplate,
                                  @Value("${platform.baseUrl}") String baseUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
    }

    @Override
    public ResponseEntity<IRunnerResponse> createAtPlatform(CommonRequestModel requestModel) throws RunnerException {
        PlatformCreateRequest request = (PlatformCreateRequest) requestModel.getData();
        String url = baseUrl + "/booking/external";
        log.info("Platform Create Request for booking reference {}: {}", request.getBooking_ref_code(), request);
        log.info("Payload sent for event: {} with request payload: {}", IntegrationType.PLATFORM_CREATE_BOOKING, jsonHelper.convertToJsonWithNulls(request));
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJsonWithNulls(request)), Object.class);
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(),0,0);
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateAtPlaform(CommonRequestModel requestModel) throws RunnerException {
        PlatformUpdateRequest request = (PlatformUpdateRequest) requestModel.getData();
        String url = baseUrl + "/notifications/booking/" + request.getBooking_reference_code();
        log.info("Endpoint:PLATFOR_UPDATE_SHIPMENT----- RequestPayload: {}", jsonHelper.convertToJson(request));
        log.info("Payload sent for event: {} with request payload: {}", IntegrationType.PLATFORM_UPDATE_BOOKING, jsonHelper.convertToJson(request));
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), Object.class);
        log.info("Endpoint:PLATFOR_UPDATE_SHIPMENT----- ResponsePayload: {}", jsonHelper.convertToJson(responseEntity));
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(),0,0);
    }
}
