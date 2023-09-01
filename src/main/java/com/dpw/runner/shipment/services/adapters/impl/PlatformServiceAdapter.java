package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformCreateRequest;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformUpdateRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;

@Service
public class PlatformServiceAdapter implements IPlatformServiceAdapter {
    private final RestTemplate restTemplate;
    private final String baseUrl;

    @Autowired
    public PlatformServiceAdapter(@Qualifier("restTemplateForPlatform") RestTemplate restTemplate,
                                  @Value("${platform.baseUrl}") String baseUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
    }

    @Override
    public ResponseEntity<?> createAtPlatform(CommonRequestModel requestModel) throws Exception {
        PlatformCreateRequest request = (PlatformCreateRequest) requestModel.getData();
        String url = baseUrl + "/booking/external";
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.post(URI.create(url)).build(), Object.class);
        return responseEntity;
    }

    @Override
    public ResponseEntity<?> updateAtPlaform(CommonRequestModel requestModel) throws Exception {
        PlatformUpdateRequest request = (PlatformUpdateRequest) requestModel.getData();
        String url = baseUrl + "/notifications/booking/" + request.getBooking_reference_code();
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(request), Object.class);
        return responseEntity;
    }
}
