package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(ReferenceNumbersConstants.REFERENCE_NUMBERS_API_HANDLE)
@Slf4j
public class ReferenceNumbersController {
    private final IReferenceNumbersService referenceNumbersService;

    @Autowired
    public ReferenceNumbersController(IReferenceNumbersService referenceNumbersService) {
        this.referenceNumbersService = referenceNumbersService;
    }

    private static class MyResponseClass extends RunnerResponse<ReferenceNumbersResponse>{}
    private static class MyListResponseClass extends RunnerListResponse<ReferenceNumbersResponse>{}


    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createReferenceNumbersData(@RequestBody @Valid ReferenceNumbersRequest request) {
        String responseMsg;
        try {
            return referenceNumbersService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return referenceNumbersService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL, responseContainer = ReferenceNumbersConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return referenceNumbersService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ReferenceNumbersConstants.REFERENCE_NUMBERS_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return referenceNumbersService.retrieveById(CommonRequestModel.buildRequest(request));
    }
    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class,  message = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid ReferenceNumbersRequest request) {
        String responseMsg;
        try {
            return referenceNumbersService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
