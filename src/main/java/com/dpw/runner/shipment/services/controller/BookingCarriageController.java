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
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.BookingCarriageMapper;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.mapstruct.factory.Mappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

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
    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> createBookingCarriageData(@RequestBody @Valid Object request) {
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
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Optional<Long> id, @RequestParam @Valid Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return (ResponseEntity<RunnerResponse>) bookingCarriageService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_LIST_SUCCESSFUL, responseContainer = BookingCarriageConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<BookingCarriageResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<BookingCarriageResponse>>) bookingCarriageService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> retrieveById(@ApiParam(value = BookingCarriageConstants.BOOKING_CARRIAGE_ID, required = true) @RequestParam Optional<Long> id, @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) bookingCarriageService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid Object request) {
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
            BookingCarriageMapper mapper = Mappers.getMapper(BookingCarriageMapper.class);
            BookingCarriageRequest req = objectMapper.convertValue(request, BookingCarriageRequest.class);
            return (ResponseEntity<RunnerResponse>) bookingCarriageService.partialUpdate(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
