package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.DetachPacksListDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackContainerNumberChangeRequest;
import com.dpw.runner.shipment.services.dto.request.AutoCalculatePackingRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.AutoCalculatePackingResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
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
    private final IPackingService packingService;
    private final JsonHelper jsonHelper;

    private static class MyResponseClass extends RunnerResponse<PackingResponse>{}
    private static class MyListResponseClass extends RunnerListResponse<PackingResponse>{}

    @Autowired
    public PackingController(IPackingService packingService, JsonHelper jsonHelper) {
        this.packingService = packingService;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_UPLOAD)
    public ResponseEntity<IRunnerResponse> uploadCSV(@ModelAttribute BulkUploadRequest request) throws IOException {
        if (request.getFile().isEmpty()) {
            return ResponseHelper.buildFailedResponse("No File Found !");
        }

        try {
            packingService.uploadPacking(request);
            return ResponseHelper.buildSuccessResponse(ApiConstants.API_UPLOAD_PACKING_DETAILS_SUCCESS_MESSAGE);
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
            packingService.downloadPacking(response, request);
        } catch (Exception ex) {
            log.error(ex.getMessage());
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = PackingConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(value = ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody PackingRequest request) {
        String responseMessage;
        try {
            PackingRequest req = jsonHelper.convertValue(request, PackingRequest.class);
            return packingService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }

        return ResponseHelper.buildFailedResponse(responseMessage);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_LIST_SUCCESSFUL, response = MyListResponseClass.class)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return packingService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(value = ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody PackingRequest request) {
        String responseMessage;
        try {
            return packingService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            return ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        String responseMessage;
        try {
            return packingService.delete(CommonRequestModel.buildRequest(id));
        } catch (Exception e) {
            responseMessage = e.getMessage();
            return ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    @PostMapping("/calculate-weight-volumne")
    public ResponseEntity<?> calculateWeightVolume(@RequestBody PackContainerNumberChangeRequest request) throws RunnerException {
        String responseMsg;
        try {
            return (ResponseEntity<?>) packingService.calculateWeightVolumne(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error in calculations";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PackingConstants.PACKING_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_LIST_PACKS_TO_DETACH)
    public ResponseEntity<?> listPacksToDetach(@RequestBody @Valid DetachPacksListDto request) {
        String responseMsg = "failure executing :(";
        try {
            return packingService.listPacksToDetach(CommonRequestModel.buildRequest(request));
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
            return packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(request), true);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Packings";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.BULK_SYNC)
    public ResponseEntity<?> syncBulkPackingToService(@RequestBody @Valid BulkPackingRequestV2 request) {
        String responseMsg = "failure executing :(";
        try {
            return packingService.V1BulkPackingCreateAndUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Container";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CALCULATION_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.AUTO_CALCULATE_VOLUMETRIC_WEIGHT)
    public ResponseEntity<?> autoCalculateVolumetricWeight(@RequestBody AutoCalculatePackingRequest request) {
        String responseMsg = "failure executing request " + request.getId();
        try {
            return packingService.autoCalculateVolumetricWeight(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error in auto calculate volmetric weight";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CALCULATION_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.AUTO_CALCULATE_CHARGABLE)
    public ResponseEntity<?> autoCalculateChargable(@RequestBody AutoCalculatePackingRequest request) {
        String responseMsg = "failure executing request " + request.getId();
        try {
            return packingService.autoCalculateChargable(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error in auto calculate chargeable";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CALCULATION_SUCCESSFUL, response = AutoCalculatePackingResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.AUTO_CALCULATE_VOLUME)
    public ResponseEntity<?> autoCalculateVolume(@RequestBody AutoCalculatePackingRequest request) {
        String responseMsg = "failure executing request " + request.getId();
        try {
            return packingService.autoCalculateVolume(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error in auto calculate volume";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CALCULATION_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.CALCULATE_VOLUMETRIC_WEIGHT)
    public ResponseEntity<?> calculateVolumetricWeight(@RequestBody AutoCalculatePackingRequest request) throws RunnerException {
        String responseMsg;
        try {
            return (ResponseEntity<?>) packingService.calculateVolumetricWeightForAirAndChargeable(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error in calculations";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
