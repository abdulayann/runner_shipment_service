package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.GetMatchingRulesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequestV3;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import java.util.Optional;
import javax.validation.Valid;

import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE_V3)
@Slf4j
public class ShipmentControllerV3 {

    @Autowired
    private IDpsEventService dpsEventService;

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
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long shipmentId,
                                              @RequestHeader(value = "x-source", required = false) String xSource) {
        return ResponseHelper.buildSuccessResponse(shipmentService.getAllMasterData(shipmentId, xSource));
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
        request.setFromConsolidation(false);
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.FETCH_MATCHING_RULES_SUCCESS, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.MATCHING_RULES_BY_GUID)
    public ResponseEntity<IRunnerResponse> getMatchingRulesByGuid(@ApiParam(value = ShipmentConstants.SHIPMENT_GUID, required = true) @RequestParam String shipmentGuid) {
        return dpsEventService.getShipmentMatchingRulesByGuid(shipmentGuid);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.FETCH_MATCHING_RULES_WITH_EXECUTION_STATE_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.MATCHING_RULES_BY_GUID_AND_EXECUTION_STATE)
    public ResponseEntity<IRunnerResponse> getMatchingRulesByGuidAndExecutionState(@RequestBody @Valid GetMatchingRulesRequest getMatchingRulesRequest) {
        return dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = RunnerListResponse.class, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_CONSOLE_SHIPMENT_LIST)
    public ResponseEntity<IRunnerResponse> consoleShipmentList(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false) Long consoleId,
            @RequestParam(required = false) String consoleGuid, @RequestParam(required = true) boolean isAttached,
            @RequestParam(required = false, defaultValue = "false") boolean getMasterData, @RequestParam(required = false, defaultValue = "false") boolean fromNte) {
        log.info("Received Console Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            return shipmentService.consoleShipmentList(CommonRequestModel.buildRequest(listCommonRequest), consoleId, consoleGuid, isAttached, getMasterData, fromNte);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.AIB_ACTION, response = UpstreamDateUpdateResponse.class)})
    @PutMapping(ApiConstants.AIB_ACTION)
    public ResponseEntity<IRunnerResponse> aibAction(@RequestBody AibActionShipment request) {
        log.info("{} | Request received for :/aib/action on shipments with body: {}", LoggerHelper.getRequestIdFromMDC(),  jsonHelper.convertToJson(request));
        try {
            return shipmentService.aibAction(request);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.REQUESTED_INTER_BRANCH_CONSOLE, response = RunnerResponse.class)})
    @GetMapping(ShipmentConstants.AIB_PUSH_REQUEST)
    public ResponseEntity<IRunnerResponse> aibPushRequest(@RequestParam() Long shipId,
                                                          @RequestParam() Long consoleId,
                                                          @RequestParam(required = false) String rejectRemarks) {
        log.info("{} | Request received for :/aib/push on shipments with ShipmentId: {} | ConsoleId: {} | Remarks: {}", LoggerHelper.getRequestIdFromMDC(), shipId, consoleId, rejectRemarks);
        try {
            return shipmentService.aibPushRequest(shipId, consoleId, rejectRemarks);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/attach-list-shipment")
    public ResponseEntity<IRunnerResponse> attachListShipment(@Valid @RequestBody @NonNull AttachListShipmentRequest request) {
        try {
            return shipmentService.attachListShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.NOTIFICATION_FETCHED_SUCCESSFULLY, response = PendingNotificationResponse.class)})
    @PostMapping(ApiConstants.AIB_NOTIFICATIONS)
    public ResponseEntity<IRunnerResponse> aibPendingNotifications(@RequestBody AibNotificationRequest request) {
        log.info("{} Request received for aibPendingNotifications for shipment {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        try {
            return shipmentService.aibPendingNotification(CommonRequestModel.builder().data(request).build());
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.OCEAN_DG_EMAIL_SEND_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.OCEAN_DG_SEND_FOR_APPROVAL)
    public ResponseEntity<IRunnerResponse> oceanDGSendForApproval(@RequestBody OceanDGApprovalRequest request) throws RunnerException {
        log.info("Received for oceanDGSendForApproval with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String warning = shipmentService.sendOceanDGApprovalEmail(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.OCEAN_DG_APPROVAL_REQUEST_RESPONSE, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.OCEAN_DG_APPROVAL_RESPONSE)
    public ResponseEntity<IRunnerResponse> oceanDGApprovalResponse(@RequestBody OceanDGRequestV3 request)
        throws RunnerException {
        log.info("Received for oceanDGApprovalResponse with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));

        String warning = shipmentService.dgApprovalResponse(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

}
