package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.platform.BookingPlatformUpdateRequest;
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
        //TODO:: Abhishek modify the api request and url for create
        PlatformCreateRequest request = (PlatformCreateRequest) requestModel.getData();
        String url = baseUrl + "??";
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.post(URI.create(url)).build(), Object.class);
        return responseEntity;
    }

    @Override
    public ResponseEntity<?> updateAtPlaform(CommonRequestModel requestModel) throws Exception {
        BookingPlatformUpdateRequest request = (BookingPlatformUpdateRequest) requestModel.getData();
        PlatformUpdateRequest platformUpdateRequest = generatePlatformRequest(request);
        String url = baseUrl + "booking/external";
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(platformUpdateRequest), Object.class);
        return responseEntity;
    }

    private PlatformUpdateRequest generatePlatformRequest(BookingPlatformUpdateRequest request) {
        // logic for conversion
        return null;
    }
}
