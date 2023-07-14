package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@SuppressWarnings("ALL")
@Slf4j
@RestController
@RequestMapping(value = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_HANDLE)
public class PickupDeliveryDetailsController {
    @Autowired
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<PickupDeliveryDetailsResponse>> createPickupDeliveryDetailsData(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<PickupDeliveryDetailsResponse>>) pickupDeliveryDetailsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<PickupDeliveryDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) pickupDeliveryDetailsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_LIST_SUCCESSFUL, responseContainer = PickupDeliveryDetailsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<PickupDeliveryDetailsResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<PickupDeliveryDetailsResponse>>) pickupDeliveryDetailsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<PickupDeliveryDetailsResponse>> retrieveById(@ApiParam(value = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<PickupDeliveryDetailsResponse>>) pickupDeliveryDetailsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) pickupDeliveryDetailsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
