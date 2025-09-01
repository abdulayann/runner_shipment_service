package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(ShippingInstructionsConstants.SI_API_HANDLE)
@Slf4j
public class ShippingInstructionsController {

    @Autowired
    private final IShippingInstructionsService service;

    @Autowired
    private JsonHelper jsonHelper;

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<SailingInformationResponse> {
    }

    public ShippingInstructionsController(IShippingInstructionsService service) {
        this.service = service;
    }

    @PostMapping
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShippingInstructionsConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = MyResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody ShippingInstructionRequest info) {
        ShippingInstructionResponse response = service.createShippingInstruction(info);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @GetMapping("/{id}")
    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ShippingInstructionsConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    public ResponseEntity<IRunnerResponse> getById(@PathVariable Long id) {
        ShippingInstructionResponse response = service.getShippingInstructionsById(id);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @PutMapping
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShippingInstructionsConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> update(@RequestBody ShippingInstructionRequest info) {
        ShippingInstructionResponse response = service.updateShippingInstructions(info);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @DeleteMapping("/{id}")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShippingInstructionsConstants.DELETE_SUCCESSFUL, response = MyResponseClass.class)})
    public ResponseEntity<IRunnerResponse> delete(@PathVariable Long id) {
        service.deleteShippingInstructions(id);
        return ResponseHelper.buildSuccessResponse();
    }
}
