package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AchievedQuantitiesRequest;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAchievedQuantitiesService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_API_HANDLE)
public class AchievedQuantitiesController {

    @Autowired
    IAchievedQuantitiesService achievedQuantitiesService;


    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<RunnerResponse<AchievedQuantitiesResponse>> create(@RequestBody @Valid @NonNull AchievedQuantitiesRequest request) {
        try {
            return (ResponseEntity<RunnerResponse<AchievedQuantitiesResponse>>) achievedQuantitiesService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return (ResponseEntity<RunnerResponse<AchievedQuantitiesResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_UPDATE_SUCCESSFUL)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity update(@RequestBody @Valid @NonNull AchievedQuantitiesRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) achievedQuantitiesService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) achievedQuantitiesService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity retrieve(@RequestParam @NonNull Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<AchievedQuantitiesResponse>>) achievedQuantitiesService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_LIST_SUCCESSFUL, responseContainer = AchievedQuantitiesConstants.ACHIEVED_QUANTITIES_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity list(@RequestBody @NonNull ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<AchievedQuantitiesResponse>>) achievedQuantitiesService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }
}
