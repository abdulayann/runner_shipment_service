package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping(ReferenceNumbersConstants.REFERENCE_NUMBERS_API_HANDLE)
@Slf4j
public class ReferenceNumbersController {
    @Autowired
    private IReferenceNumbersService referenceNumbersService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = ReferenceNumbersConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ReferenceNumbersResponse>> createReferenceNumbersData(@RequestBody @Valid ReferenceNumbersRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ReferenceNumbersResponse>>) referenceNumbersService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ReferenceNumbersResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_DELETE_SUCCESSFUL) })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) referenceNumbersService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL, responseContainer = ReferenceNumbersConstants.RESPONSE_CONTAINER_LIST) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ReferenceNumbersResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ReferenceNumbersResponse>>) referenceNumbersService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ReferenceNumbersResponse>> retrieveById(@ApiParam(value = ReferenceNumbersConstants.REFERENCE_NUMBERS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ReferenceNumbersResponse>>) referenceNumbersService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ReferenceNumbersRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) referenceNumbersService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
