package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.BookingCarriageConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.BookingCarriagePatchRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilter;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.resource.beans.container.spi.AbstractCdiBeanContainer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


import javax.validation.Valid;
import java.util.List;


@SuppressWarnings("ALL")
@RestController
@RequestMapping(BookingCarriageConstants.BOOKING_CARRIAGE_API_HANDLE)
@Slf4j
public class BookingCarriageController {
    @Autowired
    private IBookingCarriageService bookingCarriageService;
    @Autowired
    ObjectMapper objectMapper;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> createBookingCarriageData(@RequestBody @Valid BookingCarriageRequest request) {
        String responseMsg;
        try {
            BookingCarriageRequest req = objectMapper.convertValue(request, BookingCarriageRequest.class);
            return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) bookingCarriageService.create(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) bookingCarriageService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_LIST_SUCCESSFUL, responseContainer = BookingCarriageConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<BookingCarriageResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<BookingCarriageResponse>>) bookingCarriageService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> retrieveById(@ApiParam(value = BookingCarriageConstants.BOOKING_CARRIAGE_ID, required = true) @RequestParam Long id,@RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) bookingCarriageService.retrieveById(CommonRequestModel.buildRequest(request));
    }
        @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid BookingCarriageRequest request) {
        String responseMsg;
        try {
            BookingCarriageRequest req = objectMapper.convertValue(request, BookingCarriageRequest.class);
            return (ResponseEntity<RunnerResponse>) bookingCarriageService.update(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PatchMapping("/partialUpdate")
    public ResponseEntity<RunnerResponse> partialUpdate(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            BookingCarriagePatchRequest req = objectMapper.convertValue(request, BookingCarriagePatchRequest.class);
            return (ResponseEntity<RunnerResponse>) bookingCarriageService.partialUpdate(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
