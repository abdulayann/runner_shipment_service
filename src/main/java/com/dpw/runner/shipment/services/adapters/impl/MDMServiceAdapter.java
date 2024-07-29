package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.commons.dto.v1.request.CreateShipmentTaskFromBookingTaskRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.support.RetryTemplate;
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

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    ObjectMapper objectMapper;

    @Value("${mdm.creditDetails}")
    String creditConfigUrl;

    @Value("${mdm.createShipmentTaskFromBooking}")
    String createShipmentTaskFromBookingUrl;

    RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    public MDMServiceAdapter(@Qualifier("restTemplateForMDM") RestTemplate restTemplate,
                                @Value("${mdm.baseUrl}") String baseUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getCreditInfo(CommonRequestModel commonRequestModel) throws RunnerException {
        String url = baseUrl + creditConfigUrl;
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
                    if (Boolean.FALSE.equals(firstDataObject.get("isRestricted")))
                        return CustomerBookingConstants.MDM_FINAL_STATUS_NO_APPROVAL_NEEDED;
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

    @Override
    public ResponseEntity<IRunnerResponse> createShipmentTaskFromBooking(CommonRequestModel commonRequestModel) throws RunnerException {
        String url = baseUrl + createShipmentTaskFromBookingUrl;
        CreateShipmentTaskFromBookingTaskRequest request = (CreateShipmentTaskFromBookingTaskRequest) commonRequestModel.getData();
        try {
            log.info("Calling MDM createShipmentTaskFromBooking api for requestId : {} Request for {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            var resp = retryTemplate.execute((RetryCallback<ResponseEntity<IRunnerResponse>, Exception>) context -> {
                ResponseEntity<DependentServiceResponse> response = restTemplate.exchange(
                        RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)),
                        DependentServiceResponse.class
                );
                return ResponseHelper.buildDependentServiceResponse(response.getBody(), 0, 0);
            });
            log.info("MDM createShipmentTaskFromBooking api response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(resp)));
            return resp;
        } catch (Exception ex) {
            log.error("MDM Credit Details Failed due to: {}", jsonHelper.convertToJson(ex.getMessage()));
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

}
