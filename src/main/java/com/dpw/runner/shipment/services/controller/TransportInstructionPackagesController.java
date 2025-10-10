package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.TransportInstructionConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsPackagesService;
import com.fasterxml.jackson.core.JsonProcessingException;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.lang.reflect.InvocationTargetException;

@SuppressWarnings("ALL")
@Slf4j
@RestController
@RequestMapping(value = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_API_HANDLE)
public class TransportInstructionPackagesController {
    @Autowired
    private ITransportInstructionLegsPackagesService transportInstructionLegsPackagesService;

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.create(request));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE_BULK)
    public ResponseEntity<IRunnerResponse> bulkCreate(@RequestBody @Valid TransportInstructionLegsPackagesListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.bulkCreate(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@Parameter(description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_ID, required = true) @RequestParam @Valid Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.delete(id));
    }

    @ApiResponses(value = {
            @ApiResponse( responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_LIST_SUCCESSFUL, content = @Content( array = @ArraySchema(schema = @Schema(implementation = TransportInstructionLegsPackagesResponse.class))))})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        TransportInstructionLegsPackagesListResponse legsListResponse = transportInstructionLegsPackagesService.list(listCommonRequest, getMasterData);
        return ResponseHelper.buildSuccessResponse(legsListResponse, legsListResponse.getTotalPages(), legsListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_ID, required = true) @RequestParam Long id) {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.retrieveById(id));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.update(request));
    }

}
