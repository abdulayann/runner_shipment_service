package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.ContainerConstants.ASSIGN_CONTAINERS;
import static com.dpw.runner.shipment.services.commons.constants.ContainerConstants.ASSIGN_SUCCESS;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;

@RestController
@RequestMapping(ContainerConstants.CONTAINER_V3_API_HANDLE)
@Slf4j
public class ContainerV3Controller {

    private final JsonHelper jsonHelper;
    private final IContainerV3Service containerV3Service;
    private final ContainerV3Util containerV3Util;

    private static class ContainerNumberCheckResponseClass extends RunnerResponse<ContainerNumberCheckResponse>{}

    public ContainerV3Controller(JsonHelper jsonHelper, IContainerV3Service containerV3Service, ContainerV3Util containerV3Util) {
        this.jsonHelper = jsonHelper;
        this.containerV3Service = containerV3Service;
        this.containerV3Util = containerV3Util;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, response = ConsolidationDetailsResponse.class),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.SHIPMENT + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createFromShipment(@Valid @RequestBody ContainerV3Request containerRequest) {
        log.info("Received Container Create request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(containerRequest));
        return ResponseHelper.buildSuccessResponse(containerV3Service.create(containerRequest, SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, response = ConsolidationDetailsResponse.class),
        @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.CONSOLIDATION + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createFromConsolidation(@Valid @RequestBody ContainerV3Request containerRequest) {
        log.info("Received Container Create request from Consolidation with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(containerRequest));
        return ResponseHelper.buildSuccessResponse(containerV3Service.create(containerRequest, CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL, response = BulkContainerResponse.class)})
    @PutMapping(value = ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulk(@RequestBody List<ContainerV3Request> request) {
        return ResponseHelper.buildSuccessResponse(containerV3Service.updateBulk(request, SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_DELETE_SUCCESSFUL, response = BulkContainerResponse.class)})
    @DeleteMapping(value = ApiConstants.API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> deleteBulk(@RequestBody List<ContainerV3Request> request) {
        return ResponseHelper.buildSuccessResponse(containerV3Service.deleteBulk(request, SHIPMENT));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_VALIDATED, response = ContainerV3Controller.ContainerNumberCheckResponseClass.class) })
    @PostMapping(ApiConstants.API_VALIDATE_CONTAINER_NUMBER)
    public ResponseEntity<IRunnerResponse> validateContainerNumber(@RequestParam String containerNumber) {
        return ResponseHelper.buildSuccessResponse(containerV3Service.validateContainerNumber(containerNumber));
    }

    @GetMapping(ApiConstants.API_DOWNLOAD)
    public void downloadCSV(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException {
        containerV3Util.downloadContainers(response, request);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.CALCULATE_CONTAINER_SUMMARY)
    public ResponseEntity<IRunnerResponse> calculateContainerSummary(@RequestParam (required = false) Long shipmentId,
                                                                     @RequestParam (required = false) Long consolidationId,
                                                                     @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(containerV3Service.calculateContainerSummary(shipmentId, consolidationId, xSource));
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, response = ContainerListResponse.class)})
    @PostMapping(ContainerConstants.SHIPMENT_CONTAINERS)
    public ResponseEntity<IRunnerResponse> fetchShipmentContainers(@RequestBody @Valid ListCommonRequest listCommonRequest,
                                                                   @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        ContainerListResponse containerListResponse = containerV3Service.fetchShipmentContainers(CommonRequestModel.buildRequest(listCommonRequest), xSource);
        return ResponseHelper.buildSuccessResponse(containerListResponse, containerListResponse.getTotalPages(), containerListResponse.getNumberOfRecords());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, response = ContainerListV3Response.class)})
    @PostMapping(ContainerConstants.GET_CONTAINERS)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid @NonNull ListCommonRequest listCommonRequest,
                                                @RequestParam(required = false, defaultValue = "true") boolean getMasterData,
                                                @RequestHeader(value = "x-source", required = false) String xSource)
        throws RunnerException {
        ContainerListResponse containerListResponse = containerV3Service.list(listCommonRequest, getMasterData, xSource);
        return ResponseHelper.buildSuccessResponse(containerListResponse,
            containerListResponse.getTotalPages() , containerListResponse.getNumberOfRecords());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ASSIGN_SUCCESS, response = ContainerResponse.class)})
    @PostMapping(ASSIGN_CONTAINERS)
    public ResponseEntity<IRunnerResponse> assignContainers(@RequestBody @Valid AssignContainerRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(containerV3Service.assignContainers(request));
    }

}
