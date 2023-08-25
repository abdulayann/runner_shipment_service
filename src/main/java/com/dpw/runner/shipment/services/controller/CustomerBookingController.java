package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.platform.BookingPlatformUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import io.swagger.annotations.ApiParam;
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
@RequestMapping(value = CustomerBookingConstants.Customer_Booking_API_HANDLE)
public class CustomerBookingController {

    @Autowired
    private ICRPServiceAdapter crpService;

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


    @PostMapping(CustomerBookingConstants.CRP_LIST)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<?> listCRPService(@RequestBody @Valid CRPListRequest request) {
        String responseMsg;
        CRPListRequest listRequest = jsonHelper.convertValue(request, CRPListRequest.class);
        try {
            return (ResponseEntity<RunnerResponse>) crpService.listCRPService(CommonRequestModel.buildRequest(listRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(CustomerBookingConstants.CRP_RETRIEVE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.RETRIEVE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<?> retrieveCRPService(@RequestBody @Valid CRPRetrieveRequest request) {
        String responseMsg;
        CRPRetrieveRequest retrieveRequest = jsonHelper.convertValue(request, CRPRetrieveRequest.class);
        try {
            return (ResponseEntity<RunnerResponse>) crpService.retrieveCRPService(CommonRequestModel.buildRequest(retrieveRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<CustomerBookingResponse>> create(@RequestBody @Valid CustomerBookingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<CustomerBookingResponse>>) customerBookingService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<CustomerBookingResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) customerBookingService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.LIST_SUCCESSFUL, responseContainer = CustomerBookingConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<CustomerBookingResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<CustomerBookingResponse>>) customerBookingService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<CustomerBookingResponse>> retrieveById(@ApiParam(value = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<CustomerBookingResponse>>) customerBookingService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE_BOOKING)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid CustomerBookingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) customerBookingService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

}
