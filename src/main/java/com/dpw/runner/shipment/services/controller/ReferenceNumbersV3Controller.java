package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.BulkReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersV3Service;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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

    private static class MyResponseClass extends RunnerResponse<ReferenceNumbersResponse> {
    }

    private static class MyListResponseClass extends RunnerListResponse<ReferenceNumbersResponse> {
    }

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        log.info("Received Reference Number Create request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.create(request));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        log.info("Received Reference Number Update request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.update(request));

    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = BulkReferenceNumbersResponse.class)))})
    @PutMapping(value = ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulk(@RequestBody List<ReferenceNumbersRequest> request) {
        BulkReferenceNumbersResponse response = referenceNumbersV3Service.updateBulk(request);
        List<ReferenceNumbersResponse> responseList = response.getReferenceNumbersResponseList();
        List<IRunnerResponse> runnerResponseList = responseList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(runnerResponseList);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestBody @Valid ReferenceNumbersRequest request) {
        log.info("Received Reference Number Delete request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.delete(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyListResponseClass.class)), description = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest,
                                                @RequestHeader(value = "x-source", required = false) String xSource) {
        List<ReferenceNumbersResponse> referenceNumbersList = referenceNumbersV3Service.list(listCommonRequest, xSource);
        List<IRunnerResponse> responseList = referenceNumbersList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyListResponseClass.class)), description = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST_V3)
    public ResponseEntity<IRunnerResponse> listV3(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest,
                                                  @RequestHeader(value = "x-source", required = false) String xSource) {
        return referenceNumbersV3Service.listReferenceNumbers(listCommonRequest, xSource);
    }

}