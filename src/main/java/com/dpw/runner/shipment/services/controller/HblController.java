package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(HblConstants.HBL_API_HANDLE)
@Slf4j
public class HblController {
    private final IHblService hblService;
    private class MyResponseClass extends RunnerResponse<HblResponse> {}

    @Autowired
    public HblController(IHblService hblService){
        this.hblService = hblService;
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBL_GENERATION_SUCCESS, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    @PostMapping(HblConstants.API_GENERATE_HBL)
    public ResponseEntity<IRunnerResponse> generateHBL(@RequestBody @Valid HblGenerateRequest request) {
        String responseMsg;
        try {
            return hblService.generateHBL(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Seal validation completed successfully")
    })
    @GetMapping("/validate-seals/{shipmentId}")
    public ResponseEntity<IRunnerResponse> validateSealNumbers(
            @PathVariable Long shipmentId) {
        try {
            return hblService.validateSealNumberWarning(shipmentId);
        } catch (Exception e) {
            log.error("Error validating seal numbers: {}", e.getMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBL_UPDATE_SUCCESS, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid HblRequest request) {
        String responseMsg;
        try {
            return hblService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBL_DELETE_SUCCESS, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return hblService.delete(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBLS_RETRIEVE_BY_ID_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = HblConstants.HBL_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return hblService.retrieveById(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBLS_RETRIEVE_BY_ID_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    @GetMapping(HblConstants.API_RETRIEVE_BY_SHIPMENT_ID)
    public ResponseEntity<IRunnerResponse> retrieveByShipmentId(@Parameter(description = HblConstants.HBL_SHIPMENT_ID, required = true) @RequestParam Long shipmentId) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(shipmentId).build();
            return hblService.retrieveByShipmentId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBL_RESET_SUCCESSFULL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    @PostMapping(HblConstants.API_RESET_HBL)
    public ResponseEntity<IRunnerResponse> resetHbl(@RequestBody @Valid HblResetRequest request) {
        String responseMsg;
        try {
            return hblService.resetHbl(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ConsolidationConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_SAVE_FROM_V1)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> saveV1Hbl(@RequestBody @Valid HblRequestV2 request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        String responseMsg;
        try {
            return hblService.saveV1Hbl(CommonRequestModel.buildRequest(request), checkForSync);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = HblConstants.HBL_GENERATION_SUCCESS, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    @PostMapping(HblConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<IRunnerResponse> partialUpdateHBL(@RequestBody @Valid HblGenerateRequest request) {
        String responseMsg;
        try {
            return hblService.partialUpdateHBL(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
