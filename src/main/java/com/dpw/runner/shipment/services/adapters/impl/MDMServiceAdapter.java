package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class MDMServiceAdapter implements IMDMServiceAdapter {
    private final RestTemplate restTemplate;
    private final String baseUrl;
    private final String creditDetailsUrl;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    ObjectMapper objectMapper;

    public MDMServiceAdapter(@Qualifier("restTemplateForMDM") RestTemplate restTemplate,
                                @Value("${mdm.baseUrl}") String baseUrl,
                                @Value("${mdm.creditDetails}") String creditConfigUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
        this.creditDetailsUrl = creditConfigUrl;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getCreditInfo(CommonRequestModel commonRequestModel) throws RunnerException {
        String url = baseUrl + creditDetailsUrl;
        ApprovalPartiesRequest request = (ApprovalPartiesRequest) commonRequestModel.getData();
        try {
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), Object.class);
            return ResponseHelper.buildDependentServiceResponse(response.getBody(), 0, 0);
        }catch (Exception ex){
            log.error("MDM Credit Details Failed due to : {}" , jsonHelper.convertToJson(ex.getMessage()));
            throw new RunnerException("Error from MDM while fetching credit limit: " + ex.getMessage());
        }
    }

    @Override
    public String getApprovalStausForParties(CommonRequestModel commonRequestModel) throws RunnerException {
        ResponseEntity<?> response = this.getCreditInfo(commonRequestModel);
        if(response.getStatusCode().equals(HttpStatus.OK)) {
            try {
                DependentServiceResponse responseBody = objectMapper.convertValue(response.getBody(), DependentServiceResponse.class);
                LinkedHashMap<String,Object> dataList = (LinkedHashMap<String, Object>) responseBody.getData();
                String finalStatus = null;
                if (dataList != null && !dataList.isEmpty()) {
                    Map<String, Object> firstDataObject = ((List<Map<String, Object>>) dataList.get("data")).get(0);
                    finalStatus = (String) firstDataObject.get("finalStatus");
                }
                log.info("MDM Request {}" , jsonHelper.convertToJson(commonRequestModel));
                log.info("MDM Response {}", jsonHelper.convertToJson(responseBody));
                return finalStatus;
            }catch (Exception ex){
                log.error("Error getting the approval status for the parties : {}", commonRequestModel.getData());
                log.error("ERROR : {}", ex.getMessage());
            }
        }
        return null;
    }

}
