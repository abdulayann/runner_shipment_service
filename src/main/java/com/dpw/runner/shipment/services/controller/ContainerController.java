package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ContainerPackAssignDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;
import java.util.List;

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
    public ResponseEntity<String> uploadCSV(@RequestParam("file") MultipartFile file) throws IOException {
        if (file.isEmpty()) {
            return ResponseEntity.badRequest().body("No File Found !");
        }

        try {
            containerService.uploadContainers(file);
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

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL) })
    @PutMapping(ApiConstants.API_LIST_CONTAINERS_TO_ASSIGN)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> getContainersForSelection(@RequestBody ContainerAssignRequest containerAssignRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CALCULATION_SUCCESSFUL) })
    @PutMapping(ApiConstants.API_CHANGE_UNIT_ALLOCATED_ACHIEVED)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> calculateAchieved_AllocatedForSameUnit(@RequestBody ContainerRequest containerRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(containerRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CALCULATION_SUCCESSFUL) })
    @PutMapping(ApiConstants.API_CALCULATE_ACHIEVED_PACK_ASSIGN)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> calculateAchievedOnPackAssign(@RequestBody ContainerPackAssignDetachRequest containerPackAssignDetachRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.calculateAchievedQuantity_onPackAssign(CommonRequestModel.buildRequest(containerPackAssignDetachRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CALCULATION_SUCCESSFUL) })
    @PutMapping(ApiConstants.API_CALCULATE_ACHIEVED_PACK_DETACH)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> calculateAchievedOnPackDetach(@RequestBody ContainerPackAssignDetachRequest containerPackAssignDetachRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(containerPackAssignDetachRequest));
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
