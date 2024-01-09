package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackADInShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
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

    @Autowired
    JsonHelper jsonHelper;

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

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_EVENTS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_UPLOAD_EVENTS)
    public ResponseEntity<String> uploadEventsCSV(@ModelAttribute BulkUploadRequest request) throws IOException {
        if (request.getFile().isEmpty()) {
            return ResponseEntity.badRequest().body("No File Found !");
        }

        try {
            containerService.uploadContainerEvents(request);
            return ResponseEntity.ok("CSV file uploaded successfully!");
        } catch (Exception e) {
            String responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }
        return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).body("CSV File upload failed");
    }

    @GetMapping(ApiConstants.API_DOWNLOAD)
    public void downloadCSV(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) {
        try {
            containerService.downloadContainers(response, request);
        } catch (Exception ex) {
            log.error(ex.getMessage());
        }
    }

    @PostMapping(ApiConstants.EXPORT_LIST)
    public ResponseEntity<String> exportContainers(HttpServletResponse response, @RequestBody ExportContainerListRequest request) {
        try {
            containerService.exportContainers(response, request);
            return ResponseEntity.ok("Export Successfull");
        } catch (Exception ex) {
            log.error(ex.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ex.getMessage());
        }
    }

    @GetMapping(ApiConstants.API_DOWNLOAD_EVENTS)
    public void downloadEventsCSV(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) {
        try {
            containerService.downloadContainerEvents(response, request);
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
            ContainerRequest req = jsonHelper.convertValue(request, ContainerRequest.class);
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
        CommonGetRequest request = CommonGetRequest.builder().id(shipmentId).build();
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.list(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL) })
    @PostMapping(ApiConstants.API_LIST_CONTAINERS_TO_ASSIGN)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> getContainersForSelection(@RequestBody ContainerAssignListRequest containerAssignRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL) })
    @PostMapping(ApiConstants.API_CHANGE_UNIT_ALLOCATED_ACHIEVED)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> calculateAchieved_AllocatedForSameUnit(@RequestBody ContainerRequest containerRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(containerRequest));
    }

//    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL) })
//    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_PACK_ASSIGN)
//    public ResponseEntity<RunnerListResponse<ContainerResponse>> calculateAchievedOnPackAssign(@RequestBody ContainerPackADInShipmentRequest containerPackAssignDetachRequest) {
//        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.calculateAchievedQuantity_onPackAssign(CommonRequestModel.buildRequest(containerPackAssignDetachRequest));
//    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_DETACH_SUCCESSFUL) })
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_PACK_DETACH)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> calculateAchievedOnPackDetach(@RequestBody ContainerPackADInShipmentRequest containerPackAssignDetachRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(containerPackAssignDetachRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_VALIDATED) })
    @PostMapping(ApiConstants.API_VALIDATE_CONTAINER_NUMBER)
    public ResponseEntity<?> validateContainerNumber(@RequestParam String containerNumber) {
        return (ResponseEntity<?>) containerService.validateContainerNumber(containerNumber);
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

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    public ResponseEntity<?> syncContainerToService(@RequestBody @Valid ContainerRequestV2 request){
        String responseMsg = "failure executing :(";
        try {
            return containerService.V1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(request), true);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Container";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.BULK_SYNC)
    public ResponseEntity<?> syncBulkContainerToService(@RequestBody @Valid BulkContainerRequestV2 request) {
        String responseMsg = "failure executing :(";
        try {
            return containerService.V1BulkContainerCreateAndUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Container";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = EventConstants.EVENT_LIST_SUCCESS)})
    @PostMapping(ContainerConstants.GET_CONTAINERS)
    public ResponseEntity<RunnerListResponse<EventsResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<EventsResponse>>) containerService.getContainers(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.SUCCESS)})
    @GetMapping(ContainerConstants.CHECK_CONTAINERS_DELETE)
    public ResponseEntity<RunnerListResponse<EventsResponse>> checkForDelete(@RequestParam Long containerId) {
        return (ResponseEntity<RunnerListResponse<EventsResponse>>) containerService.checkForDelete(CommonRequestModel.buildRequest(containerId));
    }
}
