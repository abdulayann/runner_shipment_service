package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = ContainerConstants.CONTAINER_API_HANDLE)
public class ContainerController {

    @Autowired
    IContainerService containerService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_UPLOAD)
    public ResponseEntity<String> uploadCSV(@ModelAttribute BulkUploadRequest request) throws IOException {
        if (request.getFile().isEmpty()) {
            return ResponseEntity.badRequest().body("No File Found !");
        }

        try {
            containerService.uploadContainers(request);
            return ResponseEntity.ok("CSV file uploaded successfully!");
        } catch (Exception e) {
            String responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }
        return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).body("CSV File upload failed");
    }

    @GetMapping(ApiConstants.API_DOWNLOAD)
    public void downloadCSV(HttpServletResponse response) {
        try {
            containerService.downloadContainers(response);
        } catch (Exception ex) {
            log.error(ex.getMessage());
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(value = ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ContainerResponse>> create(@RequestBody ContainerRequest request) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse<ContainerResponse>>) containerService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }

        return (ResponseEntity<RunnerResponse<ContainerResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> list(@RequestParam Long shipmentId) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.list(CommonRequestModel.buildRequest(shipmentId));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL)})
    @PutMapping(value = ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse<ContainerResponse>> update(@RequestBody ContainerRequest request) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse<ContainerResponse>>) containerService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            return (ResponseEntity<RunnerResponse<ContainerResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse>) containerService.delete(CommonRequestModel.buildRequest(id));
        } catch (Exception e) {
            responseMessage = e.getMessage();
            return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMessage);
        }
    }
}
