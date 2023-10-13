package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IJobService;
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

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(JobConstants.JOB_API_HANDLE)
public class JobController {

    @Autowired
    private IJobService jobService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = JobConstants.JOB_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(value = ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<JobResponse>> create(@RequestBody JobRequest request) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse<JobResponse>>) jobService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }

        return (ResponseEntity<RunnerResponse<JobResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = JobConstants.JOB_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<JobResponse>> list(@RequestParam Long shipmentId) {
        return (ResponseEntity<RunnerListResponse<JobResponse>>) jobService.list(CommonRequestModel.buildRequest(shipmentId));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = EventConstants.EVENTS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<EventsResponse>> retrieveById(@ApiParam(value = JobConstants.JOB_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<EventsResponse>>) jobService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = JobConstants.JOB_UPDATE_SUCCESSFUL)})
    @PutMapping(value = ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse<JobResponse>> update(@RequestBody JobRequest request) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse<JobResponse>>) jobService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            return (ResponseEntity<RunnerResponse<JobResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = JobConstants.JOB_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse>) jobService.delete(CommonRequestModel.buildRequest(id));
        } catch (Exception e) {
            responseMessage = e.getMessage();
            return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMessage);
        }
    }


}
