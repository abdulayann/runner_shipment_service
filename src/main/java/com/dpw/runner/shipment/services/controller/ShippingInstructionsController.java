package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(ShippingInstructionsConstants.SI_API_HANDLE)
@Slf4j
public class ShippingInstructionsController {

    private final IShippingInstructionsService service;

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<SailingInformationResponse> {
    }

    @Autowired
    public ShippingInstructionsController(IShippingInstructionsService service, JsonHelper jsonHelper) {
        this.service = service;
    }

    @PostMapping
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody ShippingInstructionRequest info) {
        try {
            ShippingInstructionResponse response = service.createShippingInstruction(info);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error creating Shipping Instruction";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @GetMapping("/{id}")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class, description = ShippingInstructionsConstants.RETRIEVE_BY_ID_SUCCESSFUL)))})
    public ResponseEntity<IRunnerResponse> getById(@PathVariable Long id) {
        try {
            ShippingInstructionResponse response = service.getShippingInstructionsById(id);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error retrieving Shipping Instruction by ID";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @PutMapping
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> update(@RequestBody ShippingInstructionRequest info) {
        try {
            ShippingInstructionResponse response = service.updateShippingInstructions(info);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error updating Shipping Instruction";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @DeleteMapping("/{id}")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
    public ResponseEntity<IRunnerResponse> delete(@PathVariable Long id) {
        try {
            service.deleteShippingInstructions(id);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error deleting Shipping Instruction";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long shippingInstructionId) {
        String responseMsg = "failure executing";
        try {
            return service.getAllMasterData(shippingInstructionId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Carrier Booking LIST request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return service.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

    @GetMapping
    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = ShippingInstructionsConstants.RETRIEVE_DEFAULT_SUCCESS)})
    public ResponseEntity<IRunnerResponse> getDefault(@RequestParam Long entityId, @RequestParam EntityType type) {
        try {
            ShippingInstructionResponse response = service.getDefaultShippingInstructionValues(type, entityId);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error retrieving default Shipping Instruction";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @PostMapping("/submit/{id}")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.SUBMIT_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.ERROR_MESSAGE, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))
    })
    public ResponseEntity<IRunnerResponse> submitSI(@PathVariable("id") Long id) {
        try {
            service.submitShippingInstruction(id);
            return ResponseHelper.buildSuccessResponse(ShippingInstructionsConstants.SUBMIT_SUCCESSFUL);
        } catch (Exception ex) {
            String responseMsg = ex.getMessage() != null ? ex.getMessage() : "Error submitting Shipping Instruction";
            log.error(responseMsg, ex);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @PostMapping("/amend/{id}")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.AMEND_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.ERROR_MESSAGE, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))
    })
    public ResponseEntity<IRunnerResponse> amendSI(@PathVariable("id") Long id) {
        try {
            service.amendShippingInstruction(id);
            return ResponseHelper.buildSuccessResponse(ShippingInstructionsConstants.AMEND_SUCCESSFUL);
        } catch (Exception ex) {
            String responseMsg = ex.getMessage() != null ? ex.getMessage() : "Error amending Shipping Instruction";
            log.error(responseMsg, ex);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShippingInstructionsConstants.CANCELLED),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PutMapping(ApiConstants.CANCEL)
    // @PreAuthorize("hasAuthority('" + PermissionConstants.CARRIER_BOOKING_CANCEL + "')")
    public ResponseEntity<IRunnerResponse> cancel(@RequestParam Long id) {
        log.info("Received Carrier Booking Cancel request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        service.cancelShippingInstruction(id);
        log.info("Carrier Booking Cancel successful with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        return ResponseHelper.buildSuccessResponse();
    }

}
