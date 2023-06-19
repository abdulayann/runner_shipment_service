package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.BookingCarriageConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageGetRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(BookingCarriageConstants.BOOKING_CARRIAGE_API_HANDLE)
@Slf4j
public class BookingCarriageController {
    @Autowired
    private IBookingCarriageService bookingCarriageService;

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = BookingCarriageConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(BookingCarriageConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> createBookingCarriageData(@RequestBody @Valid BookingCarriageRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) bookingCarriageService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_DELETE_SUCCESSFUL) })
    @DeleteMapping(BookingCarriageConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        BookingCarriageGetRequest request = BookingCarriageGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) bookingCarriageService.delete(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = { @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_LIST_SUCCESSFUL, responseContainer = BookingCarriageConstants.RESPONSE_CONTAINER_LIST) })
    @PostMapping(BookingCarriageConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<BookingCarriageResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<BookingCarriageResponse>>) bookingCarriageService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = { @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(BookingCarriageConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> retrieveById(@ApiParam(value = BookingCarriageConstants.BOOKING_CARRIAGE_ID, required = true) @RequestParam Long id) {
        BookingCarriageGetRequest request = BookingCarriageGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) bookingCarriageService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = { @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(BookingCarriageConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid BookingCarriageRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) bookingCarriageService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
