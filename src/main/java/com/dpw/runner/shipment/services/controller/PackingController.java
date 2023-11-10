package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
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
@SuppressWarnings(value = "ALL")
@RestController
@RequestMapping(value = PackingConstants.PACKING_API_HANDLE)
public class PackingController {
    @Autowired
    private IPackingService packingService;

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
            packingService.uploadPacking(request);
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
            packingService.downloadPacking(response, request);
        } catch (Exception ex) {
            log.error(ex.getMessage());
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = PackingConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(value = ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<PackingResponse>> create(@RequestBody PackingRequest request) {
        String responseMessage;
        try {
            PackingRequest req = jsonHelper.convertValue(request, PackingRequest.class);
            return (ResponseEntity<RunnerResponse<PackingResponse>>) packingService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }

        return (ResponseEntity<RunnerResponse<PackingResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<PackingResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<PackingResponse>>) packingService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL)})
    @PutMapping(value = ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse<PackingResponse>> update(@RequestBody PackingRequest request) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse<PackingResponse>>) packingService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            return (ResponseEntity<RunnerResponse<PackingResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        String responseMessage;
        try {
            return (ResponseEntity<RunnerResponse>) packingService.delete(CommonRequestModel.buildRequest(id));
        } catch (Exception e) {
            responseMessage = e.getMessage();
            return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @PostMapping("/calculate-weight-volumne")
    public ResponseEntity<RunnerResponse<ContainerResponse>> calculateWeightVolume(@RequestBody PackingRequest packingRequest) throws Exception {
        String responseMsg;
        return (ResponseEntity<RunnerResponse<ContainerResponse>>) packingService.calculateWeightVolumne(CommonRequestModel.buildRequest(packingRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PackingConstants.PACKING_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_LIST_PACKS_TO_DETACH)
    public ResponseEntity<?> listPacksToDetach(@RequestParam @Valid Long containerId) {
        String responseMsg = "failure executing :(";
        try {
            return packingService.listPacksToDetach(CommonRequestModel.buildRequest(containerId));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error listing packings";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    public ResponseEntity<?> syncPackingToService(@RequestBody @Valid PackingRequestV2 request) {
        String responseMsg = "failure executing :(";
        try {
            return packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Packings";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
