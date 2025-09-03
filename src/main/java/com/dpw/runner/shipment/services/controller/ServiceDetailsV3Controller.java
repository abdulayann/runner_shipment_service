package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ServiceDetailsConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkServiceDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IServiceDetailsV3Service;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping(ServiceDetailsConstants.SERVICE_DETAILS_V3_API_HANDLE)
@Slf4j
public class ServiceDetailsV3Controller {

    private static class MyResponseClass extends RunnerResponse<ServiceDetailsResponse>{}

    private IServiceDetailsV3Service serviceDetailsV3Service;

    public ServiceDetailsV3Controller(IServiceDetailsV3Service serviceDetailsV3Service) {
        this.serviceDetailsV3Service = serviceDetailsV3Service;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.SERVICE_DETAILS_CREATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PostMapping(ApiConstants.SHIPMENT + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createFromShipment(@Valid @RequestBody ServiceDetailsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(serviceDetailsV3Service.create(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.SERVICE_DETAILS_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.SHIPMENT + ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> deleteFromShipment(@RequestParam @Valid Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(serviceDetailsV3Service.delete(id, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.SERVICE_DETAILS_UPDATE_SUCCESSFUL, response = BulkServiceDetailsResponse.class)})
    @PutMapping(value = ApiConstants.SHIPMENT + ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulkFromShipment(@RequestBody List<ServiceDetailsRequest> request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(serviceDetailsV3Service.updateBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.SERVICE_DETAILS_DELETE_SUCCESSFUL, response = BulkServiceDetailsResponse.class)})
    @DeleteMapping(value = ApiConstants.SHIPMENT + ApiConstants.API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> deleteBulkFromShipment(@RequestBody List<ServiceDetailsRequest> request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(serviceDetailsV3Service.deleteBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.SERVICE_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL, response = MyResponseClass.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = Constants.ID) @RequestParam(required = false) Long id,
                                                        @ApiParam(value = Constants.GUID) @RequestParam(required = false) String guid,
                                                        @RequestHeader(value = "x-source", required = false) String xSource) {
        return ResponseHelper.buildSuccessResponse(serviceDetailsV3Service.retrieveById(id, guid, xSource));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.SERVICE_DETAILS_LIST_SUCCESSFUL, response = ServiceDetailsListResponse.class)})
    @PostMapping(ApiConstants.SHIPMENT_SERVICES)
    public ResponseEntity<IRunnerResponse> fetchShipmentServices(@RequestBody @Valid ListCommonRequest listCommonRequest,
                                                                 @RequestHeader(value = "x-source", required = false) String xSource) {
        ServiceDetailsListResponse serviceDetailsListResponse = serviceDetailsV3Service.fetchShipmentServices(listCommonRequest, xSource);
        return ResponseHelper.buildSuccessResponse(serviceDetailsListResponse, serviceDetailsListResponse.getTotalPages(), serviceDetailsListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ServiceDetailsConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@ApiParam(value = Constants.ID, required = true) @RequestParam Long id,
                                                            @RequestHeader(value = "x-source", required = false) String xSource) {
        return ResponseHelper.buildSuccessResponse(serviceDetailsV3Service.getAllMasterData(id, xSource));
    }

}
