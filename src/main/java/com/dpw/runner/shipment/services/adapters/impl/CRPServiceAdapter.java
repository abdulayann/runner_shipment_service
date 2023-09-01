package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.*;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import java.net.URI;

@Service
@Slf4j
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
        log.info("Retrieve CRP: with request: {}", request.toString());
        ResponseEntity<?> responseEntity;
        try {
            responseEntity = restTemplate.exchange(RequestEntity.get(URI.create(url)).build(), Object.class);
        }  catch (HttpClientErrorException.NotFound ex) {
            responseEntity = ResponseHelper.buildSuccessResponse();
        }
        log.info("Retrieve CRP: with response: {}", responseEntity);
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(),0,0);
    }

    public ResponseEntity<?> listCRPService(CommonRequestModel requestModel) throws Exception {
        CRPListRequest request = (CRPListRequest) requestModel.getData();
        log.info("List CRP: with request: {}", request.toString());
        String url = crpServiceListUrl + request.getSearchString() + (request.isBillable() ? CustomerBookingConstants.BILLABLE_IDENTIFIER : StringUtility.getEmptyString());
        log.info("List CRP: To Url: {}", url);
        ResponseEntity<?> responseEntity;
        try {
            responseEntity = restTemplate.exchange(RequestEntity.get(URI.create(url)).build(), Object.class);
        } catch (HttpClientErrorException.NotFound ex) {
            responseEntity = ResponseHelper.buildSuccessResponse();
        }
        log.info("List CRP: with response: {}", responseEntity);
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(),0,0);
    }
}
