package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.request.AutoAttachConsolidationV3Request;
import com.dpw.runner.shipment.services.dto.request.CalculateAchievedValueRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.response.ConsolidationPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.fasterxml.jackson.databind.JsonMappingException;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

@RestController
@RequestMapping(ConsolidationConstants.CONSOLIDATION_V3_API_HANDLE)
@Slf4j
public class ConsolidationV3Controller {

    private final IConsolidationV3Service consolidationV3Service;
    private final JsonHelper jsonHelper;

    private static class MyResponseClass extends RunnerResponse<ConsolidationDetailsV3Response> {}
    private static class MyListResponseClass extends RunnerListResponse<ConsolidationDetailsV3Response> {}

    @Autowired
    public ConsolidationV3Controller(IConsolidationV3Service consolidationV3Service,
                                     JsonHelper jsonHelper) {
        this.consolidationV3Service = consolidationV3Service;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ConsolidationConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ConsolidationV3Controller.MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull ConsolidationDetailsV3Request request) {
        log.info("Received Consolidation create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.create(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ConsolidationConstants.UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ConsolidationV3Controller.MyResponseClass.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ConsolidationDetailsV3Request request) throws RunnerException {
        log.info("Received Consolidation update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.completeUpdate(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = ConsolidationV3Controller.MyResponseClass.class, description = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam (required = false) Long id,
                                                        @Parameter(description = ShipmentConstants.SHIPMENT_GUID) @RequestParam (required = false) String guid,
                                                        @RequestHeader(value = "x-source", required = false) String xSource
    ) throws RunnerException, AuthenticationException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).guid(guid).build();
        log.info("Received Consolidation retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.retrieveById(request, xSource));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class, description = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)))})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long consolidationId,
                                                            @RequestHeader(value = "x-source", required = false) String xSource
    ) throws RunnerException, AuthenticationException{
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.getAllMasterData(consolidationId, xSource));
    }

    /**
     * Attaches shipments to a consolidation.
     *
     * @param request Shipment attach request
     * @return Standard runner response
     */
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class, description = ConsolidationConstants.ATTACH_SHIPMENT_SUCCESSFUL)))
    })
    @PostMapping(ApiConstants.ATTACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> attachShipments(@RequestBody @Valid ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        log.info("Received attachShipments request: {}", request);
        request.setFromConsolidation(true);
        String warning = consolidationV3Service.attachShipments(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class, description = ConsolidationConstants.DETACH_SUCCESSFUL)))})
    @PostMapping(ApiConstants.DETACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> detachShipments(@RequestBody @Valid ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        log.info("Received detachShipments request: {} with RequestId: {}", request, LoggerHelper.getRequestIdFromMDC());
        return consolidationV3Service.detachShipments(request);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class, description = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)))})
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_VALUES)
    public ResponseEntity<IRunnerResponse> calculateAchievedValues(@RequestBody CalculateAchievedValueRequest request) throws RunnerException {
        ShipmentGridChangeV3Response response = consolidationV3Service.calculateAchievedValues(request);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @GetMapping(ApiConstants.API_RETRIEVE_PENDING_NOTIFICATION_DATA)
    public ResponseEntity<IRunnerResponse> pendingNotificationsData(@Parameter(description = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam Long id) {
        log.info("Received pending notification consolidation data v3 request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        ConsolidationPendingNotificationResponse consolidationPendingNotificationResponse =  consolidationV3Service.getPendingNotificationData(request);
        return ResponseHelper.buildSuccessResponse(consolidationPendingNotificationResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = MyListResponseClass.class))), description = ConsolidationConstants.LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST_V3)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Consolidation list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        ConsolidationListV3Response consolidationListV3Response =  consolidationV3Service.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
        return ResponseHelper.buildListSuccessConsolidationResponse(consolidationListV3Response.getConsolidationListResponses(), consolidationListV3Response.getTotalPages(),
            consolidationListV3Response.getNumberOfRecords());

    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerListResponse.class, description = "Successful Console Details Data List Retrieval")))})
    @PostMapping(value = ApiConstants.AUTO_ATTACH_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(@Valid @RequestBody @NonNull AutoAttachConsolidationV3Request request) {
        log.info("Received console list v3 for shipment request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        ConsolidationListV3Response consolidationListV3Response =   consolidationV3Service.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        return ResponseHelper.buildSuccessResponse(consolidationListV3Response, consolidationListV3Response.getTotalPages(),
                consolidationListV3Response.getNumberOfRecords());
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful Consolidation Attachment Flag Update"),
            @ApiResponse(responseCode = "400", description = "Invalid input - enableFlag cannot be null")
    })
    @PostMapping(value = ApiConstants.ATTACHMENT_FLAG)
    public ResponseEntity<IRunnerResponse> updateConsolidationAttachmentFlag(@RequestParam Boolean enableFlag, @RequestParam Long consolId) {
        consolidationV3Service.updateConsolidationAttachmentFlag(enableFlag, consolId);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class, description = ShipmentConstants.UPDATE_SAILING_SCHEDULE_SUCCESSFUL)))
    })
    @PostMapping(ApiConstants.UPDATE_SAILING_SCHEDULE)
    public ResponseEntity<IRunnerResponse> updateSailingScheduleDataToShipment(@RequestBody @Valid ConsolidationSailingScheduleRequest request) throws RunnerException {
        log.info("Received updateSailingSchedule request: {}", request);
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.updateSailingScheduleDataToShipment(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShipmentConstants.AIB_ACTION, content = @Content(schema = @Schema(implementation = UpstreamDateUpdateResponse.class)))})
    @PutMapping(ApiConstants.AIB_ACTION)
    public ResponseEntity<IRunnerResponse> aibAction(@RequestBody AibActionConsolidation request) {
        log.info("{} | Request received for :/aib/action on console with body: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        try {
            return consolidationV3Service.aibAction(request);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ConsolidationConstants.NOTIFICATION_FETCHED_SUCCESSFULLY, content = @Content(schema = @Schema(implementation = PendingNotificationResponse.class)))})
    @PostMapping(ApiConstants.AIB_NOTIFICATIONS)
    public ResponseEntity<IRunnerResponse> aibPendingNotifications(@RequestBody AibNotificationRequest request) {
        log.info("{} Request received for aibPendingNotifications for consolidation {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        try {
            return consolidationV3Service.aibPendingNotification(CommonRequestModel.builder().data(request).build());
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShipmentConstants.ALL_SHIPMENT_COUNT, content = @Content(schema = @Schema(implementation = UpstreamDateUpdateResponse.class)))})
    @GetMapping(ApiConstants.AIB_SHIPMENT_COUNT)
    public ResponseEntity<IRunnerResponse> aibAttachedPendingShipmentCount(@RequestParam() Long id,
                                                                           @RequestHeader(value = "x-source", required = false) String xSource) {
        log.info("{} Request received for aibAttachedPendingShipmentCount for consolidation: {}", LoggerHelper.getRequestIdFromMDC(), id);
        try {
            return consolidationV3Service.aibAttachedPendingShipmentCount(CommonGetRequest.builder().id(id).build(), xSource);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }
    
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class, description = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)))
    })
    @GetMapping(ApiConstants.API_GET_SYNC_ACHIEVED_DATA)
    public ResponseEntity<IRunnerResponse> getConsoleSyncAchievedData(@RequestParam Long consolidationId) throws RunnerException, JsonMappingException {
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.getConsoleSyncAchievedData(consolidationId));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShipmentConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @GetMapping(ApiConstants.GET_DG_SHIPMENT)
    public ResponseEntity<IRunnerResponse> getDGShipment(@Parameter(description = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.getDGShipment(id));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = ConsolidationV3Controller.MyResponseClass.class, description = ShipmentConstants.DEFAULT_SHIPMENT_GENERATED_SUCCESSFULLY)))
    })
    @GetMapping(ApiConstants.API_DEFAULT_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> getDefaultConsolidation() {
        ConsolidationDetailsV3Response defaultConsolidation = consolidationV3Service.getDefaultConsolidation();
        return ResponseHelper.buildSuccessResponse(defaultConsolidation);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = ConsolidationV3Controller.MyResponseClass.class, description = ConsolidationConstants.CONSOLE_DETAILS_FETCHED_SUCCESSFULLY)))})
    @GetMapping(ApiConstants.API_CONSOLE_FROM_SHIPMENT)
    public ResponseEntity<IRunnerResponse> getNewConsoleDataFromShipmentId(@Parameter(description = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) throws RunnerException, AuthenticationException {
        log.info("Received getNewConsoleDataFromShipmentId: {}" , id);
        ConsolidationDetailsV3Response defaultConsolidation = consolidationV3Service.getDefaultConsolidation();
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.getNewConsoleDataFromShipment(id, defaultConsolidation));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ConsolidationConstants.CREATE_CONSOLE_AND_ATTACHED_SHIPMENT_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ConsolidationV3Controller.MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE_CONSOLE_ATTACH_SHIPMENT)
    public ResponseEntity<IRunnerResponse> createConsoleAndAttachShipment(@RequestBody @Valid @NonNull ConsolidationDetailsV3Request request) throws RunnerException {
        log.info("Received Consolidation createConsoleAndAttachShipment request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String warning = consolidationV3Service.createConsoleDetailsAndAttachShipment(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

}
