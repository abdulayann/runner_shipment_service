package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ArrivalDepartureDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.ArrivalDepartureDetailsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IArrivalDepartureDetailsService;
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
@RequestMapping(value = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_API_HANDLE)
public class ArrivalDepartureDetailsController {

    private final IArrivalDepartureDetailsService arrivalDepartureDetailsService;

    @Autowired
    public ArrivalDepartureDetailsController(IArrivalDepartureDetailsService arrivalDepartureDetailsService) {
        this.arrivalDepartureDetailsService = arrivalDepartureDetailsService;
    }

    private class MyResponseClass extends RunnerResponse<ArrivalDepartureDetailsResponse>{}
    private class MyListResponseClass extends RunnerListResponse<ArrivalDepartureDetailsResponse>{}


    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull ArrivalDepartureDetailsRequest request) {
        try {
            return arrivalDepartureDetailsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_UPDATE_SUCCESSFUL, response = MyResponseClass.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull ArrivalDepartureDetailsRequest request) {
        String responseMsg;
        try {
            return arrivalDepartureDetailsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return arrivalDepartureDetailsService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieve(@RequestParam @NonNull Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return arrivalDepartureDetailsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_LIST_SUCCESSFUL, responseContainer = ArrivalDepartureConstants.ARRIVAL_DEPARTURE_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull ListCommonRequest listCommonRequest) {
        return arrivalDepartureDetailsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }
}
