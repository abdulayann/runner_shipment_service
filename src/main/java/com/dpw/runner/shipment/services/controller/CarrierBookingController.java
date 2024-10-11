package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.CarrierBookingResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CarrierBookingConstants.CARRIER_BOOKING_API_HANDLE)
public class CarrierBookingController {

    private ICarrierBookingService carrierBookingService;
    private class MyResponseClass extends RunnerResponse<CarrierBookingResponse> {}
    private class MyListResponseClass extends RunnerListResponse<CarrierBookingResponse> {}

    @Autowired
    public CarrierBookingController(ICarrierBookingService carrierBookingService){
        this.carrierBookingService = carrierBookingService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CarrierBookingConstants.CREATE_SUCCESSFUL, response = CarrierBookingController.MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid CarrierBookingRequest request) {
        String responseMsg;
        try {
            return carrierBookingService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CarrierBookingConstants.DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return carrierBookingService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = CarrierBookingController.MyListResponseClass.class, message = CarrierBookingConstants.LIST_SUCCESSFUL, responseContainer = CarrierBookingConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return carrierBookingService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CarrierBookingConstants.UPDATE_SUCCESSFUL, response = CarrierBookingController.MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE_BOOKING)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid CarrierBookingRequest request) {
        String responseMsg;
        try {
            return carrierBookingService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
