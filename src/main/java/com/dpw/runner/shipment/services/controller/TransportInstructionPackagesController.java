package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PickupDeliveryDetailsConstants;
import com.dpw.runner.shipment.services.commons.constants.TransportInstructionConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesListResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsPackagesService;
import com.fasterxml.jackson.core.JsonProcessingException;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
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

import javax.validation.Valid;
import java.lang.reflect.InvocationTargetException;

@SuppressWarnings("ALL")
@Slf4j
@RestController
@RequestMapping(value = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_API_HANDLE)
public class TransportInstructionPackagesController {
    @Autowired
    private ITransportInstructionLegsPackagesService transportInstructionLegsPackagesService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.create(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE_BULK)
    public ResponseEntity<IRunnerResponse> bulkCreate(@RequestBody @Valid TransportInstructionLegsPackagesListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.bulkCreate(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@ApiParam(value = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_ID, required = true) @RequestParam @Valid Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.delete(id));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_LIST_SUCCESSFUL, responseContainer = PickupDeliveryDetailsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        TransportInstructionLegsPackagesListResponse legsListResponse = transportInstructionLegsPackagesService.list(listCommonRequest, getMasterData);
        return ResponseHelper.buildSuccessResponse(legsListResponse, legsListResponse.getTotalPages(), legsListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_ID, required = true) @RequestParam Long id) {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.retrieveById(id));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = TransportInstructionConstants.TRANSPORT_INSTRUCTION_LEGS_PACKAGES_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return ResponseHelper.buildSuccessResponse(transportInstructionLegsPackagesService.update(request));
    }

}
