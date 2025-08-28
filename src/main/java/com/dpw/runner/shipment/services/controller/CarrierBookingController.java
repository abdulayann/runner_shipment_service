package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingListRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
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

@RestController
@RequestMapping(CarrierBookingConstants.CARRIER_BOOKING_API_HANDLE)
@Slf4j
public class CarrierBookingController {

    private final ICarrierBookingService carrierBookingService;
    private final JsonHelper jsonHelper;

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<CarrierBookingResponse> {}
    private static class MyListResponseClass extends RunnerListResponse<CarrierBookingListResponse> {}

    @Autowired
    public CarrierBookingController(ICarrierBookingService carrierBookingService, JsonHelper jsonHelper) {
        this.carrierBookingService = carrierBookingService;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid CarrierBookingRequest request) {
        log.info("Received Carrier Booking CREATE request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBookingResponse response = carrierBookingService.create(request);
        log.info("Carrier Booking CREATE successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_RETRIEVE_BY_ID_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> getById(@RequestParam Long id) {
        log.info("Received Carrier Booking GET BY ID request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBookingResponse response = carrierBookingService.findById(id);
        log.info("Carrier Booking GET BY ID successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerListResponse.class)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Carrier Booking LIST request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        CarrierBookingListResponse response = carrierBookingService.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
        log.info("Carrier Booking LIST successful with RequestId: {} and totalRecords: {}, totalPages: {}", LoggerHelper.getRequestIdFromMDC(), response.getNumberOfRecords(), response.getTotalPages());
        return ResponseHelper.buildListSuccessCarrierBookingResponse(
                response.getCarrierBookingResponseList(),
                response.getTotalPages(),
                response.getNumberOfRecords());
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestParam Long id,
                                                  @RequestBody @Valid CarrierBookingRequest request) {
        log.info("Received Carrier Booking UPDATE request with RequestId: {}, id: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), id, jsonHelper.convertToJson(request));
        CarrierBookingResponse response = carrierBookingService.update(id, request);
        log.info("Carrier Booking UPDATE successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CarrierBookingConstants.CARRIER_BOOKING_DELETE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam Long id) {
        log.info("Received Carrier Booking DELETE request with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        carrierBookingService.delete(id);
        log.info("Carrier Booking DELETE successful with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        return ResponseHelper.buildSuccessResponse();
    }
}