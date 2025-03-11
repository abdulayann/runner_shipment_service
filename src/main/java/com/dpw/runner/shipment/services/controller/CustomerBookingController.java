package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CreditLimitRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CustomerBookingConstants.CUSTOMER_BOOKING_API_HANDLE)
public class CustomerBookingController {

    private ICRPServiceAdapter crpService;
    private JsonHelper jsonHelper;
    private ICustomerBookingService customerBookingService;

    private class MyResponseClass extends RunnerResponse<CustomerBookingResponse> {}
    private class MyListResponseClass extends RunnerListResponse<CustomerBookingResponse> {}
    private class MyCreditLimitResponseClass extends RunnerResponse<CreditLimitResponse> {}

    @Autowired
    public CustomerBookingController(ICRPServiceAdapter crpService, JsonHelper jsonHelper, ICustomerBookingService customerBookingService){
        this.crpService = crpService;
        this.jsonHelper = jsonHelper;
        this.customerBookingService = customerBookingService;
    }

    @PostMapping(CustomerBookingConstants.PLATFORM_CREATE_BOOKING)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> platformCreateBooking(@RequestBody @Valid PlatformToRunnerCustomerBookingRequest request) {
        String responseMsg;
        PlatformToRunnerCustomerBookingRequest platformBookingCreateRequest = jsonHelper.convertValue(request, PlatformToRunnerCustomerBookingRequest.class);
        try {
            return customerBookingService.platformCreateBooking(CommonRequestModel.buildRequest(platformBookingCreateRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }


    @PostMapping(CustomerBookingConstants.CRP_LIST)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> listCRPService(@RequestBody @Valid CRPListRequest request) {
        String responseMsg;
        CRPListRequest listRequest = jsonHelper.convertValue(request, CRPListRequest.class);
        try {
            return crpService.listCRPService(CommonRequestModel.buildRequest(listRequest));
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
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> retrieveCRPService(@RequestBody @Valid CRPRetrieveRequest request) {
        String responseMsg;
        CRPRetrieveRequest retrieveRequest = jsonHelper.convertValue(request, CRPRetrieveRequest.class);
        try {
            return crpService.retrieveCRPService(CommonRequestModel.buildRequest(retrieveRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_CREATE + "')")
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid CustomerBookingRequest request) {
        String responseMsg;
        try {
            return customerBookingService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return customerBookingService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = CustomerBookingConstants.LIST_SUCCESSFUL, responseContainer = CustomerBookingConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return customerBookingService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = CustomerBookingConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = CustomerBookingConstants.BOOKING_ID) @RequestParam Optional<Long> id, @ApiParam(value = CustomerBookingConstants.BOOKING_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        ResponseEntity<IRunnerResponse> response = customerBookingService.retrieveById(CommonRequestModel.buildRequest(request));
        return response;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE_BOOKING)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid CustomerBookingRequest request) {
        String responseMsg;
        try {
            return customerBookingService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_CANCEL_BOOKING)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_CANCEL + "')")
    public ResponseEntity<IRunnerResponse> cancel(@RequestBody @Valid CustomerBookingRequest request) {
        String responseMsg;
        try {
            return customerBookingService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_SUCCESSFUL, response = MyCreditLimitResponseClass.class)})
    @PostMapping(CustomerBookingConstants.FUSION_CHECK_CREDIT_LIMIT)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromFusion(@RequestBody @Valid CreditLimitRequest request) {
        String responseMsg;
        try {
            return customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_SUCCESSFUL, response = V1ShipmentCreationResponse.class)})
    @GetMapping(CustomerBookingConstants.RETRY_FOR_BILLING)
    public ResponseEntity<IRunnerResponse> retryForBilling(@ApiParam(value = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) {
        String responseMsg;
        try {
            return customerBookingService.retryForBilling(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(id).build()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.API_CLONE)
    public ResponseEntity<IRunnerResponse> cloneById(@ApiParam(value = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return customerBookingService.cloneBooking(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.RETRIEVE_BY_ORDER_ID_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ORDER_ID)
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(@ApiParam(value = CustomerBookingConstants.ORDER_ID, required = true) @RequestParam String orderId) throws RunnerException {
        return customerBookingService.retrieveByOrderId(orderId);
    }
}
