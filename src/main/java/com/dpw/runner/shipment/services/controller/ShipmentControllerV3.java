package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;


@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE_V3)
@Slf4j
public class ShipmentControllerV3 {

    IShipmentServiceV3 shipmentService;

    @Autowired
    public ShipmentControllerV3(IShipmentServiceV3 shipmentService) {
        this.shipmentService = shipmentService;
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.PENDING_NOTIFICATION_COUNT_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.COUNT_PENDING_NOTIFICATION_API)
    public ResponseEntity<IRunnerResponse> getPendingNotificationCount() {
        String responseMsg;
        try {
            log.info("Received shipment notification pending count request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
            return shipmentService.getPendingNotificationCount();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);

    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(
            @RequestBody @Valid ListCommonRequest listCommonRequest,
            @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        log.info("Received shipment list v3 request with RequestId: {}",
                LoggerHelper.getRequestIdFromMDC());
        return shipmentService.listShipment(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

    @GetMapping(ApiConstants.API_GET_SHIPMENT_ASSIGN_CONTAINER_TRAY)
    public ResponseEntity<IRunnerResponse> getShipmentAssignContainerTray(@ApiParam Long containerId, @ApiParam Long consolidationId)  {
        return ResponseHelper.buildSuccessResponse(shipmentService.getShipmentAndPacksForConsolidationAssignContainerTray(containerId, consolidationId));
    }

}
