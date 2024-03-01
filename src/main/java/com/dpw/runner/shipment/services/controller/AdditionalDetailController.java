package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.AdditionalDetailConstants;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
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
    private final IAdditionalDetailService additionalDetailService;

    @Autowired
    public AdditionalDetailController(IAdditionalDetailService additionalDetailService) {
        this.additionalDetailService = additionalDetailService;
    }
    private class MyResponseClass extends RunnerResponse<AdditionalDetailResponse>{}
    private class MyListResponseClass extends RunnerListResponse<AdditionalDetailResponse>{}

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createAdditionalDetailsData(@RequestBody @Valid AdditionalDetailRequest request) {
        String responseMsg;
        try {
            return additionalDetailService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return additionalDetailService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return additionalDetailService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = AdditionalDetailConstants.ADDITIONAL_DETAILS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return additionalDetailService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AdditionalDetailConstants.ADDITIONAL_DETAILS_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid BookingCarriageRequest request) {
        String responseMsg;
        try {
            return additionalDetailService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
