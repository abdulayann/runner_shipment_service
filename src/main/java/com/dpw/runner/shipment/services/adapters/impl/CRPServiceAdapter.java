package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import org.springframework.beans.factory.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.RequestEntity;

import java.net.URI;

@Service
public class CRPServiceAdapter implements com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter {

    private final RestTemplate restTemplate;
    private final String crpServiceRetrieveUrl;
    private final String crpServiceListUrl;

    @Autowired
    public CRPServiceAdapter(@Qualifier("restTemplateForCRP") RestTemplate restTemplate,
                             @Value("${crp.service.retrieve.url}") String crpServiceRetrieveUrl,
                             @Value("${crp.service.list.url}") String crpServiceListUrl) {
        this.restTemplate = restTemplate;
        this.crpServiceRetrieveUrl = crpServiceRetrieveUrl;
        this.crpServiceListUrl = crpServiceListUrl;
    }

    public ResponseEntity<?> retrieveCRPService(CommonRequestModel requestModel) throws Exception {
        CRPRetrieveRequest request = (CRPRetrieveRequest) requestModel.getData();
        String url = crpServiceRetrieveUrl + request.getSearchString();
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.get(URI.create(url)).build(), Object.class);
        return responseEntity;
    }

    public ResponseEntity<?> listCRPService(CommonRequestModel requestModel) throws Exception {
        CRPListRequest request = (CRPListRequest) requestModel.getData();
        String url = crpServiceRetrieveUrl + request.getSearchString();
        ResponseEntity<?> responseEntity = restTemplate.exchange(RequestEntity.get(URI.create(url)).build(), Object.class);
        return responseEntity;
    }
}
