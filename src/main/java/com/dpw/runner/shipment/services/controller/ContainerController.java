package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ExportContainerListRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CheckAllocatedDataChangeResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CheckAllocatedDataChangesRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackADInShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import java.io.IOException;
import java.util.List;
import java.util.Objects;

import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.ErrorResponse;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = ContainerConstants.CONTAINER_API_HANDLE)
public class ContainerController {

    private final IContainerService containerService;

    private class MyContainerResponseClass extends RunnerResponse<ContainerResponse> {}
    private class MyContainerListResponseClass extends RunnerListResponse<ContainerResponse> {}
    private class CheckAllocatedDataChangeResponseClass extends RunnerResponse<CheckAllocatedDataChangeResponse> {}
    private class ContainerNumberCheckResponseClass extends RunnerResponse<ContainerNumberCheckResponse>{}


    @Autowired
    public ContainerController(IContainerService containerService) {
        this.containerService = containerService;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerResponseClass.class))),
            @ApiResponse(responseCode = "404", description = ContainerConstants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_UPLOAD)
    public ResponseEntity<IRunnerResponse> uploadCSV(@ModelAttribute BulkUploadRequest request) throws IOException {
        if (Objects.isNull(request.getFile()) || request.getFile().isEmpty()) {
            return ResponseHelper.buildFailedResponse("No File Found !");
        }

        try {
            containerService.uploadContainers(request);
            return ResponseHelper.buildSuccessResponse(ApiConstants.API_UPLOAD_CONTAINER_DETAILS_SUCCESS_MESSAGE);
        } catch (Exception e) {
            String responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
            return ResponseHelper.buildFailedResponse(responseMessage, HttpStatus.EXPECTATION_FAILED);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_EVENTS_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class))),
            @ApiResponse(responseCode = "404", description = ContainerConstants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_UPLOAD_EVENTS)
    public ResponseEntity<IRunnerResponse> uploadEventsCSV(@ModelAttribute BulkUploadRequest request) throws IOException {
        if (Objects.isNull(request.getFile()) || request.getFile().isEmpty()) {
            return ResponseHelper.buildFailedResponse("No File Found !");
        }

        try {
            containerService.uploadContainerEvents(request);
            return ResponseHelper.buildSuccessResponse(ApiConstants.API_UPLOAD_CONTAINER_EVENTS_SUCCESS_MESSAGE);
        } catch (Exception e) {
            String responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
            return ResponseHelper.buildFailedResponse(responseMessage, HttpStatus.EXPECTATION_FAILED);
        }
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
            @ApiResponse(responseCode ="200", description = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerResponseClass.class))),
            @ApiResponse(responseCode = "404", description = ContainerConstants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(value = ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody ContainerRequest request) {
        String responseMessage;
        try {
            return containerService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }

        return ResponseHelper.buildFailedResponse(responseMessage);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerListResponseClass.class)))})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestParam Long shipmentId) {
        CommonGetRequest request = CommonGetRequest.builder().id(shipmentId).build();
        return containerService.list(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerListResponseClass.class) ))})
    @PostMapping(ApiConstants.API_LIST_CONTAINERS_TO_ASSIGN)
    public ResponseEntity<IRunnerResponse> getContainersForSelection(@RequestBody ContainerAssignListRequest containerAssignRequest) {
        return containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignRequest));
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ContainerConstants.CALCULATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerListResponseClass.class))) })
    @PostMapping(ApiConstants.API_CHANGE_UNIT_ALLOCATED_ACHIEVED)
    public ResponseEntity<IRunnerResponse> calculateAchievedAllocatedForSameUnit(@RequestBody ContainerRequest containerRequest) {
        return containerService.calculateAchievedAllocatedForSameUnit(CommonRequestModel.buildRequest(containerRequest));
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = CheckAllocatedDataChangeResponseClass.class ,description = ContainerConstants.CALCULATION_SUCCESSFUL))) })
    @PostMapping(ApiConstants.API_CHECK_ALLOCATED_DATA_CHANGE)
    public ResponseEntity<IRunnerResponse> calculateAllocatedData(@RequestBody CheckAllocatedDataChangesRequest containerRequest) {
        return containerService.calculateAllocatedData(CommonRequestModel.buildRequest(containerRequest));
    }


    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_DETACH_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerListResponse.class))) })
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_PACK_DETACH)
    public ResponseEntity<IRunnerResponse> calculateAchievedOnPackDetach(@RequestBody ContainerPackADInShipmentRequest containerPackAssignDetachRequest) {
        return containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(containerPackAssignDetachRequest));
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_VALIDATED, content = @Content(schema = @Schema(implementation = ContainerNumberCheckResponseClass.class))) })
    @PostMapping(ApiConstants.API_VALIDATE_CONTAINER_NUMBER)
    public ResponseEntity<IRunnerResponse> validateContainerNumber(@RequestParam String containerNumber) {
        return containerService.validateContainerNumber(containerNumber);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerResponseClass.class)))})
    @PutMapping(value = ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody ContainerRequest request) {
        String responseMessage;
        try {
            return containerService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            return ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        String responseMessage;
        try {
            return containerService.delete(CommonRequestModel.buildRequest(id));
        } catch (Exception e) {
            responseMessage = e.getMessage();
            return ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.SYNC)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> syncContainerToService(@RequestBody @Valid ContainerRequestV2 request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync){
        String responseMsg = "failure executing :(";
        try {
            return containerService.v1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(request), checkForSync);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Container";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.BULK_SYNC)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> syncBulkContainerToService(@RequestBody @Valid BulkContainerRequestV2 request) {
        String responseMsg = "failure executing :(";
        try {
            return containerService.v1BulkContainerCreateAndUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Container";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyContainerListResponseClass.class)))})
    @PostMapping(ContainerConstants.GET_CONTAINERS)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return containerService.getContainers(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = EventConstants.EVENT_LIST_SUCCESS, content = @Content(schema = @Schema(implementation = MyContainerListResponseClass.class)))})
    @PostMapping(ContainerConstants.LIST_BY_MODULE_GUID_AND_MODULE_TYPE)
    public ResponseEntity<IRunnerResponse> listByModuleGuidAndModuleType(@RequestParam String moduleGuid, @RequestParam String moduleType) {
        return containerService.getByModuleGuidAndModuleType(moduleGuid, moduleType);
    }


    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.SUCCESS, content = @Content(schema = @Schema(implementation = RunnerListResponse.class)))})
    @GetMapping(ContainerConstants.CHECK_CONTAINERS_DELETE)
    public ResponseEntity<IRunnerResponse> checkForDelete(@RequestParam Long containerId) {
        return containerService.checkForDelete(CommonRequestModel.buildRequest(containerId));
    }

    @PostMapping(ApiConstants.API_SYNC_CONTAINERS)
    public ResponseEntity<IRunnerResponse> getContainers(@RequestBody @Valid List<Long> request) {
        String responseMsg;
        try {
            return containerService.containerSync(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
