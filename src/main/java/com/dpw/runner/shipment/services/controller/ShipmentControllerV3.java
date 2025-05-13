package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;


@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE_V3)
@Slf4j
public class ShipmentControllerV3 {

    private static class MyResponseClass extends RunnerResponse<ShipmentDetailsV3Response> {}
    private static class ShipmentUnAssignContainerTrayList extends RunnerResponse<ShipmentPacksUnAssignContainerTrayDto> {}
    private static class ShipmentAssignContainerTrayList extends RunnerResponse<ShipmentPacksAssignContainerTrayDto> {}

    private IShipmentServiceV3 shipmentService;
    private JsonHelper jsonHelper;

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
        return ResponseHelper.buildSuccessResponse(shipmentService.getPendingNotificationCount());
    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(
            @RequestBody @Valid ListCommonRequest listCommonRequest,
            @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        log.info("Received shipment list v3 request with RequestId: {}",
                LoggerHelper.getRequestIdFromMDC());
        return shipmentService.listShipment(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = ShipmentControllerV3.MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid ShipmentV3Request request) {
        log.info("Received Shipment create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(shipmentService.create(CommonRequestModel.buildRequest(request)));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = ShipmentControllerV3.MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ShipmentV3Request request) throws RunnerException {
        log.info("Received Shipment update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(shipmentService.completeUpdate(CommonRequestModel.buildRequest(request)));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        shipmentService.delete(CommonRequestModel.buildRequest(request));
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id,
                                                        @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid,
                                                        @RequestParam(required = false, defaultValue = "false") boolean getMasterData,
                                                        @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException, AuthenticationException {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(shipmentService.retrieveById(CommonRequestModel.buildRequest(request), getMasterData, xSource));
    }

    @GetMapping(ApiConstants.API_RETRIEVE_PENDING_NOTIFICATION_DATA)
    public ResponseEntity<IRunnerResponse> pendingNotificationsData(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Long id) {
        log.info("Received pending notification shipment data v3 request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        ShipmentPendingNotificationResponse shipmentPendingNotificationResponse = shipmentService.getPendingNotificationData(request);
        return ResponseHelper.buildSuccessResponse(shipmentPendingNotificationResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<?> getAllMasterData(@RequestParam Long shipmentId,
                                              @RequestHeader(value = "x-source", required = false) String xSource) {
        return (ResponseEntity<?>) shipmentService.getAllMasterData(shipmentId, xSource);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = ShipmentAssignContainerTrayList.class)})
    @GetMapping(ApiConstants.API_GET_SHIPMENT_ASSIGN_CONTAINER_TRAY)
    public ResponseEntity<IRunnerResponse> getShipmentAssignContainerTray(@ApiParam Long containerId, @ApiParam Long consolidationId) {
        return ResponseHelper.buildSuccessResponse(shipmentService.getShipmentAndPacksForConsolidationAssignContainerTray(containerId, consolidationId));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = ShipmentUnAssignContainerTrayList.class)})
    @GetMapping(ApiConstants.API_GET_SHIPMENT_UN_ASSIGN_CONTAINER_TRAY)
    public ResponseEntity<IRunnerResponse> getShipmentUnAssignContainerTray(@ApiParam Long containerId) {
        return ResponseHelper.buildSuccessResponse(shipmentService.getShipmentAndPacksForConsolidationUnAssignContainerTray(containerId));
    }

    /**
     * Attaches consolidation to a shipment.
     *
     * @param request Consolidation attach request
     * @return Standard runner response
     */
    @ApiResponses(value = {
            @ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.ATTACH_CONSOLIDATION_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.ATTACH_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> attachConsolidation(@RequestBody @Valid ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        log.info("Received attachConsolidation request: {}", request);
        String warning = shipmentService.attachConsolidation(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }
    @ApiResponses(value = {
            @ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.UPDATE_SAILING_SCHEDULE_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.UPDATE_SAILING_SCHEDULE)
    public ResponseEntity<IRunnerResponse> updateSailingScheduleDataToShipment(@RequestBody @Valid ShipmentSailingScheduleRequest request) throws RunnerException {
        log.info("Received updateSailingSchedule request: {}", request);
        return ResponseHelper.buildSuccessResponse(shipmentService.updateSailingScheduleDataToShipment(request));
    }

}
