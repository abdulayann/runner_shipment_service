package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AirMessagingLogsRequest;
import com.dpw.runner.shipment.services.dto.response.AirMessagingLogsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
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
@RequestMapping(AirMessagingLogsConstants.AIR_MESSAGING_LOGS_API_HANDLE)
@Slf4j
public class AirMessagingLogsController {
    private final IAirMessagingLogsService airMessagingLogsService;

    @Autowired
    public AirMessagingLogsController(IAirMessagingLogsService airMessagingLogsService) {
        this.airMessagingLogsService = airMessagingLogsService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = AirMessagingLogsConstants.AIR_MESSAGING_LOGS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid AirMessagingLogsRequest request) {
        String responseMsg;
        try {
            return airMessagingLogsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = AirMessagingLogsConstants.AIR_MESSAGING_LOGS_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return airMessagingLogsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = AirMessagingLogsConstants.AIR_MESSAGING_LOGS_LIST_SUCCESSFUL, responseContainer = AirMessagingLogsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return airMessagingLogsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = AirMessagingLogsConstants.AIR_MESSAGING_LOGS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = AirMessagingLogsConstants.AIR_MESSAGING_LOGS_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return airMessagingLogsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AirMessagingLogsConstants.AIR_MESSAGING_LOGS_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid AirMessagingLogsRequest request) {
        String responseMsg;
        try {
            return airMessagingLogsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    private class MyResponseClass extends RunnerResponse<AirMessagingLogsResponse> {
    }

    private class MyListResponseClass extends RunnerListResponse<AirMessagingLogsResponse> {
    }
}
