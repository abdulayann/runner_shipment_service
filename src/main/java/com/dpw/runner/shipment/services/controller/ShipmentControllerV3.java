package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.Optional;


@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE_V3)
@Slf4j
public class ShipmentControllerV3 {

    private final IShipmentServiceV3 shipmentService;
    private final JsonHelper jsonHelper;

    @Autowired
    public ShipmentControllerV3(IShipmentServiceV3 shipmentService, JsonHelper jsonHelper) {
        this.shipmentService = shipmentService;
        this.jsonHelper = jsonHelper;
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.PENDING_NOTIFICATION_COUNT_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.COUNT_PENDING_NOTIFICATION_API)
    public ResponseEntity<IRunnerResponse> getPendingNotificationCount() {
        log.info("Received shipment notification pending count request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return shipmentService.getPendingNotificationCount();
    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(
            @RequestBody @Valid ListCommonRequest listCommonRequest,
            @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        log.info("Received shipment list v3 request with RequestId: {}",
                LoggerHelper.getRequestIdFromMDC());
        return shipmentService.listShipment(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid ShipmentRequest request) {
        log.info("Received Shipment create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(shipmentService.create(CommonRequestModel.buildRequest(request)));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ShipmentRequest request) throws RunnerException {
        log.info("Received Shipment update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(shipmentService.completeUpdate(CommonRequestModel.buildRequest(request)));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return shipmentService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.retrieveById(CommonRequestModel.buildRequest(request), getMasterData);
    }
}
