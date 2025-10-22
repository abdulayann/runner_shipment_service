package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SyncBookingToService;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingCloneResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
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

@RestController
@RequestMapping(CarrierBookingConstants.CARRIER_BOOKING_API_HANDLE)
@Slf4j
public class CarrierBookingController {

    private final ICarrierBookingService carrierBookingService;
    private final JsonHelper jsonHelper;

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<CarrierBookingResponse> {
    }


    @Autowired
    public CarrierBookingController(ICarrierBookingService carrierBookingService, JsonHelper jsonHelper) {
        this.carrierBookingService = carrierBookingService;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = CarrierBookingConstants.CARRIER_BOOKING_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(value = ApiConstants.API_CREATE)
    // @PreAuthorize("hasAuthority('" + PermissionConstants.CARRIER_BOOKING_CREATE + "')")
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid CarrierBookingRequest request) throws RunnerException {
        log.info("Received Carrier Booking CREATE request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBookingResponse response = carrierBookingService.create(request);
        log.info("Carrier Booking CREATE successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = CarrierBookingConstants.CARRIER_BOOKING_RETRIEVE_BY_ID_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    // @PreAuthorize("hasAuthority('" + PermissionConstants.CARRIER_BOOKING_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@RequestParam Long id) {
        log.info("Received Carrier Booking GET BY ID request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBookingResponse response = carrierBookingService.retrieveById(id);
        log.info("Carrier Booking GET BY ID successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @PostMapping(value = ApiConstants.API_LIST)
//    @PreAuthorize("hasAuthority('" + PermissionConstants.CARRIER_BOOKING_VIEW + "')")
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Carrier Booking LIST request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return carrierBookingService.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = CarrierBookingConstants.CARRIER_BOOKING_UPDATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PutMapping(ApiConstants.API_UPDATE)
    // @PreAuthorize("hasAuthority('" + PermissionConstants.CARRIER_BOOKING_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid CarrierBookingRequest request) throws RunnerException {
        log.info("Received Carrier Booking UPDATE request with RequestId: {}, and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBookingResponse response = carrierBookingService.update(request);
        log.info("Carrier Booking UPDATE successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CarrierBookingConstants.CARRIER_BOOKING_DELETE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam Long id) {
        log.info("Received Carrier Booking DELETE request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        carrierBookingService.delete(id);
        log.info("Carrier Booking DELETE successful with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CarrierBookingConstants.CANCELLED),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PutMapping(ApiConstants.CANCEL)
    // @PreAuthorize("hasAuthority('" + PermissionConstants.CARRIER_BOOKING_CANCEL + "')")
    public ResponseEntity<IRunnerResponse> cancel(@RequestParam Long id) {
        log.info("Received Carrier Booking Cancel request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        carrierBookingService.cancel(id);
        log.info("Carrier Booking Cancel successful with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CarrierBookingConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long carrierBookingId) {
        String responseMsg = "failure executing :(";
        try {
            return carrierBookingService.getAllMasterData(carrierBookingId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CarrierBookingConstants.CARRIER_BOOKING_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.SYNC)
    public ResponseEntity<IRunnerResponse> syncCarrierBookingToService(@RequestBody @Valid SyncBookingToService syncBookingToService) throws RunnerException {
        log.info("Received Carrier Booking Sync request with RequestId: {} and body: {}", LoggerHelper.getRequestIdFromMDC(), syncBookingToService);
        carrierBookingService.syncCarrierBookingToService(syncBookingToService);
        log.info("Carrier Booking Sync successful with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), syncBookingToService);
        return ResponseHelper.buildSuccessResponse();
    }

    @GetMapping
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CarrierBookingConstants.RETRIEVE_DEFAULT_SUCCESS)})
    public ResponseEntity<IRunnerResponse> getDefault(@RequestParam Long entityId, @RequestParam EntityType type) {
        try {
            CarrierBookingResponse response = carrierBookingService.getDefaultCarrierBookingValues(type, entityId);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error retrieving default carrier booking data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CarrierBookingConstants.SUBMIT_AMEND_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CarrierBookingController.MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = CarrierBookingController.MyResponseClass.class)))
    })
    @PostMapping(ApiConstants.API_SUBMIT_OR_AMEND)
    public ResponseEntity<IRunnerResponse> submitOrAmend(@RequestBody SubmitAmendInttraRequest submitAmendInttraRequest) throws RunnerException {
        log.info("Received Carrier Booking Request with RequestId: {} and OperationType: {}", LoggerHelper.getRequestIdFromMDC(), submitAmendInttraRequest.getOperationType());
        carrierBookingService.submitOrAmend(submitAmendInttraRequest);
        log.info("Verified Gross Mass successful with RequestId: {}, OperationType: {} and response: {}",
                LoggerHelper.getRequestIdFromMDC(), submitAmendInttraRequest.getOperationType(), jsonHelper.convertToJson(submitAmendInttraRequest));
        return ResponseHelper.buildSuccessResponse();
    }

    @GetMapping(value = ApiConstants.API_CLONE)
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CarrierBookingConstants.CLONE_CARRIER_BOOKING_SUCCESS)})
    public ResponseEntity<IRunnerResponse> cloneCarrierBooking(@RequestParam Long entityId) {
        try {
            CarrierBookingCloneResponse response = carrierBookingService.cloneBooking(entityId);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : "Error retrieving clone carrier booking data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @PostMapping(value = ApiConstants.CONSOLIDATED_API_LIST)
    public ResponseEntity<IRunnerResponse> consolidatedList(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Carrier Booking Consolidated LIST request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return carrierBookingService.consolidatedList(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }
}