package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.AdditionalDetailConstants;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAdditionalDetailService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(AdditionalDetailConstants.ADDITIONAL_DETAILS_API_HANDLE)
@Slf4j
public class AdditionalDetailController {
    @Autowired
    private IAdditionalDetailService additionalDetailService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = AdditionalDetailConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<AdditionalDetailResponse>> createAdditionalDetailsData(@RequestBody @Valid AdditionalDetailRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AdditionalDetailResponse>>) additionalDetailService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AdditionalDetailResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_DELETE_SUCCESSFUL) })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) additionalDetailService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_LIST_SUCCESSFUL) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<AdditionalDetailResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<AdditionalDetailResponse>>) additionalDetailService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<AdditionalDetailResponse>> retrieveById(@ApiParam(value = AdditionalDetailConstants.ADDITIONAL_DETAILS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<AdditionalDetailResponse>>) additionalDetailService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid BookingCarriageRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) additionalDetailService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
