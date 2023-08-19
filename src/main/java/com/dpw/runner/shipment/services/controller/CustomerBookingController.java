package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CustomerBookingConstants.Customer_Booking_API_HANDLE)
public class CustomerBookingController {
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ICustomerBookingService customerBookingService;
    @PostMapping(CustomerBookingConstants.PLATFORM_CREATE_BOOKING)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<?> platformCreateBooking(@RequestBody @Valid PlatformToRunnerCustomerBookingRequest request) {
        String responseMsg;
        PlatformToRunnerCustomerBookingRequest platformBookingCreateRequest = jsonHelper.convertValue(request, PlatformToRunnerCustomerBookingRequest.class);
        try {
            return (ResponseEntity<RunnerResponse>) customerBookingService.platformCreateBooking(CommonRequestModel.buildRequest(platformBookingCreateRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
