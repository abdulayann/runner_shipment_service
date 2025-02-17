package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.*;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.Objects;

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

    public ResponseEntity<IRunnerResponse> retrieveCRPService(CommonRequestModel requestModel) throws RunnerException {
        CRPRetrieveRequest request = (CRPRetrieveRequest) requestModel.getData();
        String url = crpServiceRetrieveUrl + (Objects.isNull(request.getSearchString()) ? StringUtility.getEmptyString() : request.getSearchString().replace(" ", ""));
        log.info("Retrieve CRP: with request: {}", request);
        ResponseEntity<?> responseEntity;
        try {
            responseEntity = restTemplate.exchange(RequestEntity.get(URI.create(url)).build(), Object.class);
        } catch (HttpClientErrorException ex) {
            responseEntity = ResponseHelper.buildSuccessResponse();
        }
        log.info("Retrieve CRP: with response: {}", responseEntity);
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(), 0, 0);
    }

    public ResponseEntity<IRunnerResponse> listCRPService(CommonRequestModel requestModel) throws RunnerException {
        CRPListRequest request = (CRPListRequest) requestModel.getData();
        log.info("List CRP: with request: {}", request.toString());
        String url = crpServiceListUrl + (Objects.isNull(request.getSearchString()) ? StringUtility.getEmptyString() : request.getSearchString().replace(" ", "%20")) + (request.isBillable() ? CustomerBookingConstants.BILLABLE_IDENTIFIER : StringUtility.getEmptyString());
        log.info("List CRP: To Url: {}", url);
        ResponseEntity<?> responseEntity;
        try {
            responseEntity = restTemplate.exchange(RequestEntity.get(URI.create(url)).build(), Object.class);
        } catch (HttpClientErrorException ex) {
            responseEntity = ResponseHelper.buildSuccessResponse();
        }
        log.info("List CRP: with response: {}", responseEntity);
        return ResponseHelper.buildDependentServiceResponse(responseEntity.getBody(), 0, 0);
    }
}
