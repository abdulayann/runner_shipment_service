package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import com.fasterxml.jackson.databind.JsonMappingException;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping(ShippingInstructionsConstants.SI_API_HANDLE)
@Slf4j
public class ShippingInstructionsController {

    private final IShippingInstructionsService service;

    private final JsonHelper jsonHelper;

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<SailingInformationResponse> {
    }

    @Autowired
    public ShippingInstructionsController(IShippingInstructionsService service, JsonHelper jsonHelper) {
        this.service = service;
        this.jsonHelper = jsonHelper;
    }

    @PostMapping
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShippingInstructionsConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = MyResponseClass.class)
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
    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ShippingInstructionsConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
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
            @ApiResponse(code = 200, message = ShippingInstructionsConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShippingInstructionsConstants.DELETE_SUCCESSFUL, response = MyResponseClass.class)})
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShippingInstructionsConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<?> getAllMasterData(@RequestParam Long shippingInstructionId) {
        String responseMsg = "failure executing :(";
        try {
            return (ResponseEntity<?>) service.getAllMasterData(shippingInstructionId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) throws JsonMappingException {
        log.info("Received Carrier Booking LIST request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return service.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

}
