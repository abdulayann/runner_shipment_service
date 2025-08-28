package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingListRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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
        CarrierBookingResponse response = carrierBookingService.create(request);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_RETRIEVE_BY_ID_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> getById(@RequestParam Long id) {
        CarrierBookingResponse response = carrierBookingService.getById(id);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerListResponse.class)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid CarrierBookingListRequest request) {
        CarrierBookingListResponse response = carrierBookingService.list(request);
        return ResponseHelper.buildListSuccessCarrierBookingResponse(response.getCarrierBookingResponseList(), response.getTotalPages(),
                response.getNumberOfRecords());
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyResponseClass.class, message = CarrierBookingConstants.CARRIER_BOOKING_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestParam Long id,
                                                  @RequestBody @Valid CarrierBookingRequest request) {
        CarrierBookingResponse response = carrierBookingService.update(id, request);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CarrierBookingConstants.CARRIER_BOOKING_DELETE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam Long id) {
        carrierBookingService.delete(id);
        return ResponseHelper.buildSuccessResponse();
    }
}
