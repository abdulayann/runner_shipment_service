package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
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

@RestController
@RequestMapping(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_API_HANDLE)
@Slf4j
public class VerifiedGrossMassController {

    private final IVerifiedGrossMassService verifiedGrossMassService;
    private final JsonHelper jsonHelper;

    public VerifiedGrossMassController(IVerifiedGrossMassService verifiedGrossMassService, JsonHelper jsonHelper) {
        this.verifiedGrossMassService = verifiedGrossMassService;
        this.jsonHelper = jsonHelper;
    }

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<VerifiedGrossMassResponse> {
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid VerifiedGrossMassRequest request) {
        log.info("Received Verified Gross Mass CREATE request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        VerifiedGrossMassResponse response = verifiedGrossMassService.create(request);
        log.info("Verified Gross Mass CREATE successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_RETRIEVE_BY_ID_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@RequestParam Long id) {
        log.info("Received Verified Gross Mass GET BY ID request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        VerifiedGrossMassResponse response = verifiedGrossMassService.retrieveById(id);
        log.info("Verified Gross Mass GET BY ID successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Verified Gross Mass LIST request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return verifiedGrossMassService.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid VerifiedGrossMassRequest request) {
        log.info("Received Verified Gross Mass UPDATE request with RequestId: {}, and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        VerifiedGrossMassResponse response = verifiedGrossMassService.update(request);
        log.info("Verified Gross Mass UPDATE successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_DELETE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam Long id) {
        log.info("Received Verified Gross Mass DELETE request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        verifiedGrossMassService.delete(id);
        log.info("Verified Gross Mass DELETE successful with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = VerifiedGrossMassConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<?> getAllMasterData(@RequestParam Long vgmId) {
        String responseMsg = "failure executing :(";
        try {
            return (ResponseEntity<?>) verifiedGrossMassService.getAllMasterData(vgmId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }
    @GetMapping
    @ApiResponses(value = {@ApiResponse(code = 200, message = VerifiedGrossMassConstants.RETRIEVE_DEFAULT_SUCCESS)})
    public ResponseEntity<IRunnerResponse> getDefault(@RequestParam Long entityId, @RequestParam EntityType type) {
        try {
            VerifiedGrossMassResponse response = verifiedGrossMassService.getDefaultVerifiedGrossMassValues(type, entityId);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error retrieving default verified gross mass data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}