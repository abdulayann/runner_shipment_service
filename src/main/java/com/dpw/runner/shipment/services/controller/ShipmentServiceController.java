package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentServiceConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentServiceRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentServiceResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServicesService;
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
@RequestMapping(ShipmentServiceConstants.SHIPMENT_SERVICE_API_HANDLE)
@Slf4j
public class ShipmentServiceController {
    @Autowired
    private IShipmentServicesService shipmentServicesService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentServiceConstants.SHIPMENT_SERVICE_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = ShipmentServiceConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ShipmentServiceResponse>> createShipmentServiceData(@RequestBody @Valid ShipmentServiceRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ShipmentServiceResponse>>) shipmentServicesService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ShipmentServiceResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentServiceConstants.SHIPMENT_SERVICE_DELETE_SUCCESSFUL) })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) shipmentServicesService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentServiceConstants.SHIPMENT_SERVICE_LIST_SUCCESSFUL, responseContainer = ShipmentServiceConstants.RESPONSE_CONTAINER_LIST) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ShipmentServiceResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ShipmentServiceResponse>>) shipmentServicesService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentServiceConstants.SHIPMENT_SERVICE_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentServiceResponse>> retrieveById(@ApiParam(value = ShipmentServiceConstants.SHIPMENT_SERVICE_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ShipmentServiceResponse>>) shipmentServicesService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentServiceConstants.SHIPMENT_SERVICE_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ShipmentServiceRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentServicesService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
