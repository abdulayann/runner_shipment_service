package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentSettingsConstants;
import com.dpw.runner.shipment.services.commons.constants.TruckDriverDetailsConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ViewsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITruckDriverDetailsService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;
import org.springframework.beans.factory.annotation.Autowired;

import javax.validation.Valid;

import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NO_DATA;

@RestController
@RequestMapping(TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_API_HANDLE)
@Slf4j
public class TruckDriverDetailsController {
    private final ITruckDriverDetailsService truckDriverDetailsService;

    private class MyResponseClass extends RunnerResponse<TruckDriverDetailsResponse>{}
    private class MyListResponseClass extends RunnerListResponse<TruckDriverDetailsResponse>{}
    @Autowired
    public TruckDriverDetailsController(ITruckDriverDetailsService truckDriverDetailsService) {
        this.truckDriverDetailsService = truckDriverDetailsService;
    }

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull TruckDriverDetailsRequest request) {
        try {
            return truckDriverDetailsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_UPDATE_SUCCESSFUL)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull TruckDriverDetailsRequest request) {
        String responseMsg;
        try {
            return truckDriverDetailsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_DELETE_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return truckDriverDetailsService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieve(@RequestParam @NonNull Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return truckDriverDetailsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_LIST_SUCCESSFUL, responseContainer = TruckDriverDetailsConstants.TRUCK_DRIVER_DETAILS_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        return truckDriverDetailsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

}
