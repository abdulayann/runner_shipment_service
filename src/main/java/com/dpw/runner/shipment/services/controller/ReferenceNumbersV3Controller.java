package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.BulkReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersV3Service;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;

@Slf4j
@RestController
@RequestMapping(value = ReferenceNumbersConstants.REFERENCE_NUMBERS_V3_API_HANDLE)
public class ReferenceNumbersV3Controller {
    private final IReferenceNumbersV3Service referenceNumbersV3Service;
    private final JsonHelper jsonHelper;


    @Autowired
    public ReferenceNumbersV3Controller(IReferenceNumbersV3Service referenceNumbersV3Service, JsonHelper jsonHelper) {
        this.referenceNumbersV3Service = referenceNumbersV3Service;
        this.jsonHelper = jsonHelper;
    }

    private static class MyResponseClass extends RunnerResponse<ReferenceNumbersResponse> {}
    private static class MyListResponseClass extends RunnerListResponse<ReferenceNumbersResponse> {}

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        log.info("Received Reference Number Create request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.create(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, response = MyResponseClass.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        log.info("Received Reference Number Update request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.update(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, response = BulkReferenceNumbersResponse.class)})
    @PutMapping(value = ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulk(@RequestBody List<ReferenceNumbersRequest> request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.updateBulk(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestBody @Valid ReferenceNumbersRequest request) {
        log.info("Received Reference Number Delete request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.delete(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL, responseContainer = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest,
                                                @RequestHeader(value = "x-source", required = false) String xSource) {
        List<ReferenceNumbersResponse> referenceNumbersList = referenceNumbersV3Service.list(listCommonRequest, xSource);
        List<IRunnerResponse> responseList = referenceNumbersList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

}
