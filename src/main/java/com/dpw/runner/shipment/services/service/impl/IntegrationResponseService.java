package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.shipment.services.dto.response.IntegrationResponsesResponse;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IIntegrationResponseService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Slf4j
@Service
public class IntegrationResponseService implements IIntegrationResponseService {

    @Autowired
    IIntegrationResponseDao integrationResponseDao;

    @Autowired
    private JsonHelper jsonHelper;


    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }


    @Override
    public ResponseEntity<IRunnerResponse> fetchIntegrationResponses(CommonRequestModel commonRequestModel) {
        IntegrationResponseRequest request = (IntegrationResponseRequest) commonRequestModel.getData();
        List<IntegrationResponse> responses = integrationResponseDao.getIntegrationResponses(request);

        return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(responses));
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<IntegrationResponse> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(integrationResponse -> {
            IntegrationResponsesResponse response = jsonHelper.convertValue(integrationResponse, IntegrationResponsesResponse.class);
            responseList.add(response);
        });
        return responseList;
    }
}
