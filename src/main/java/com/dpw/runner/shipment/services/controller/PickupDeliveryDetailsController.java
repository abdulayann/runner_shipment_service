package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PickupDeliveryDetailsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_LIST_SUCCESSFUL, responseContainer = PickupDeliveryDetailsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return pickupDeliveryDetailsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_CREATE_V2)
    public ResponseEntity<IRunnerResponse> createV2(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.createV2(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_DELETE_SUCCESSFUL)})
    @DeleteMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_DELETE_V2)
    public ResponseEntity<IRunnerResponse> deleteV2(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.deleteV2(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_LIST_SUCCESSFUL, responseContainer = PickupDeliveryDetailsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_RETRIEVE_BY_ID_V2)
    public ResponseEntity<IRunnerResponse> listV2(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return pickupDeliveryDetailsService.listV2(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_RETRIEVE_BY_ID_V2)
    public ResponseEntity<IRunnerResponse> retrieveByIdV2(@ApiParam(value = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.retrieveByIdV2(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_UPDATE_V2)
    public ResponseEntity<IRunnerResponse> updateV2(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.updateV2(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
