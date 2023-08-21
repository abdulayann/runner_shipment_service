package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.service.interfaces.INPMService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@Slf4j
public class NPMService implements INPMService {

    @Value("${NPM.BaseUrl}")
    private String npmBaseUrl;
    @Value("${NPM.Contracts}")
    private String npmContracts;
    @Value("${NPM.xApikeyV2}")
    private String xApikeyV2;
    @Autowired
    private RestTemplate restTemplate;

    @Override
    public ResponseEntity<?> fetchContracts(CommonRequestModel commonRequestModel) throws Exception {
        ListContractRequest listContractRequest = (ListContractRequest) commonRequestModel.getData();
        String url = npmBaseUrl + npmContracts;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("x-api-key-v2", xApikeyV2);

        HttpEntity<Object> request = new HttpEntity<Object>(listContractRequest, headers);

        ResponseEntity<?> response = restTemplate.exchange(url, HttpMethod.POST, request, Object.class);
        return response;
    }
}
