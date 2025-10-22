package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.TransportInstructionConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersListResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsContainersService;
import com.fasterxml.jackson.core.JsonProcessingException;
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

import java.lang.reflect.InvocationTargetException;

@SuppressWarnings("ALL")
@Slf4j
@RestController
@RequestMapping(value = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_API_HANDLE)
public class TransportInstructionContainersController {
    @Autowired
    private ITransportInstructionLegsContainersService transportInstructionLegsContainersService;

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid TransportInstructionLegsContainersRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsContainersService.create(request));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE_BULK)
    public ResponseEntity<IRunnerResponse> bulkCreate(@RequestBody @Valid TransportInstructionLegsContainersListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsContainersService.bulkCreate(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@Parameter(description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_ID, required = true) @RequestParam @Valid Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsContainersService.delete(id));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        TransportInstructionLegsContainersListResponse legsListResponse = transportInstructionLegsContainersService.list(listCommonRequest, getMasterData);
        return ResponseHelper.buildSuccessResponse(legsListResponse, legsListResponse.getTotalPages(), legsListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_ID, required = true) @RequestParam Long id) {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsContainersService.retrieveById(id));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_CONTAINERS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid TransportInstructionLegsContainersRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsContainersService.update(request));
    }

}
