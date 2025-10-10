package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PickupDeliveryDetailsConstants;
import com.dpw.runner.shipment.services.commons.constants.TransportInstructionConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsTruckDriverRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsTruckDriverService;
import com.fasterxml.jackson.core.JsonProcessingException;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;
import java.lang.reflect.InvocationTargetException;

@SuppressWarnings("ALL")
@Slf4j
@RestController
@RequestMapping(value = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_API_HANDLE)
public class TransportInstructionTruckDriverController {
    @Autowired
    private ITransportInstructionLegsTruckDriverService transportInstructionLegsTruckDriverService;

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid TransportInstructionLegsTruckDriverRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsTruckDriverService.create(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@Parameter(description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_ID, required = true) @RequestParam @Valid Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsTruckDriverService.delete(id));
    }

    @ApiResponse(
            responseCode = "200",
            description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_LIST_SUCCESSFUL,
            content = @Content(
                    mediaType = "application/json",
                    array = @ArraySchema(schema = @Schema(implementation = TransportInstructionLegsTruckDriverResponse.class))
            )
    )
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        TransportInstructionLegsTruckDriverListResponse legsListResponse = transportInstructionLegsTruckDriverService.list(listCommonRequest, getMasterData);
        return ResponseHelper.buildSuccessResponse(legsListResponse, legsListResponse.getTotalPages(), legsListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_ID, required = true) @RequestParam Long id) {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsTruckDriverService.retrieveById(id));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_TRUCK_DRIVER_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid TransportInstructionLegsTruckDriverRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsTruckDriverService.update(request));
    }

}
