package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
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
import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.ApiConstants.API_BULK_UPDATE;
import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_BULK_UPDATE_SUCCESSFUL;

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
            @ApiResponse(code = 200, response = MyResponseClass.class, message = VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_UPDATE_SUCCESSFUL),
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
        return verifiedGrossMassService.getAllMasterData(vgmId);
    }

    @GetMapping
    @ApiResponses(value = {@ApiResponse(code = 200, message = VerifiedGrossMassConstants.RETRIEVE_DEFAULT_SUCCESS)})
    public ResponseEntity<IRunnerResponse> getDefault(@RequestParam Long entityId, @RequestParam EntityType type) {
        VerifiedGrossMassResponse response = verifiedGrossMassService.getDefaultVerifiedGrossMassValues(type, entityId);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = VERIFIED_GROSS_MASS_BULK_UPDATE_SUCCESSFUL, response = CommonContainerResponse.class, responseContainer = "List")})
    @PutMapping(API_BULK_UPDATE)
    public ResponseEntity<IRunnerResponse> bulkUpdateContainers(@RequestBody @Valid VerifiedGrossMassBulkUpdateRequest request) {
        log.info("Received container bulk update request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        try {
            List<CommonContainerResponse> response = verifiedGrossMassService.bulkUpdateContainers(request);
            log.info("Container bulk update successful with RequestId: {} and updated {} containers",
                    LoggerHelper.getRequestIdFromMDC(), response.size());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (ValidationException e) {
            log.warn("Validation failed for bulk update: {}", e.getMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage());
        } catch (Exception e) {
            log.error("Error processing bulk update: {}", e.getMessage(), e);
            return ResponseHelper.buildFailedResponse("Failed to process bulk update");
        }
    }
}