package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.request.AutoAttachConsolidationV3Request;
import com.dpw.runner.shipment.services.dto.request.CalculateAchievedValueRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.response.ConsolidationPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import javax.validation.Valid;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

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
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = ConsolidationV3Controller.MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull ConsolidationDetailsV3Request request) {
        log.info("Received Consolidation create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.create(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = ConsolidationV3Controller.MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ConsolidationDetailsV3Request request) throws RunnerException {
        log.info("Received Consolidation update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.completeUpdate(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = ConsolidationV3Controller.MyResponseClass.class, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam (required = false) Long id,
                                                        @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam (required = false) String guid,
                                                        @RequestHeader(value = "x-source", required = false) String xSource
    ) throws RunnerException, AuthenticationException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).guid(guid).build();
        log.info("Received Consolidation retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.retrieveById(request, xSource));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
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
            @ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.ATTACH_SHIPMENT_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.ATTACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> attachShipments(@RequestBody @Valid ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        log.info("Received attachShipments request: {}", request);
        String warning = consolidationV3Service.attachShipments(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.DETACH_SUCCESSFUL)})
    @PostMapping(ApiConstants.DETACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> detachShipments(@RequestBody @Valid ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        log.info("Received detachShipments request: {} with RequestId: {}", request, LoggerHelper.getRequestIdFromMDC());
        return consolidationV3Service.detachShipments(request);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_VALUES)
    public ResponseEntity<IRunnerResponse> calculateAchievedValues(@RequestBody CalculateAchievedValueRequest request) throws RunnerException {
        ShipmentGridChangeV3Response response = consolidationV3Service.calculateAchievedValues(request);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @GetMapping(ApiConstants.API_RETRIEVE_PENDING_NOTIFICATION_DATA)
    public ResponseEntity<IRunnerResponse> pendingNotificationsData(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam Long id) {
        log.info("Received pending notification consolidation data v3 request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        ConsolidationPendingNotificationResponse consolidationPendingNotificationResponse =  consolidationV3Service.getPendingNotificationData(request);
        return ResponseHelper.buildSuccessResponse(consolidationPendingNotificationResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = ConsolidationConstants.LIST_SUCCESSFUL, responseContainer = ConsolidationConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST_V3)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "true") boolean getMasterData) {
        log.info("Received Consolidation list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        ConsolidationListV3Response consolidationListV3Response =  consolidationV3Service.list(listCommonRequest, getMasterData);
        return ResponseHelper.buildSuccessResponse(consolidationListV3Response, consolidationListV3Response.getTotalPages(),
            consolidationListV3Response.getNumberOfRecords());

    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = "Successful Console Details Data List Retrieval")})
    @PostMapping(value = ApiConstants.AUTO_ATTACH_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(@Valid @RequestBody @NonNull AutoAttachConsolidationV3Request request) {
        log.info("Received console list v3 for shipment request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        ConsolidationListV3Response consolidationListV3Response =   consolidationV3Service.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        return ResponseHelper.buildSuccessResponse(consolidationListV3Response, consolidationListV3Response.getTotalPages(),
                consolidationListV3Response.getNumberOfRecords());
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Successful Consolidation Attachment Flag Update"),
            @ApiResponse(code = 400, message = "Invalid input - enableFlag cannot be null")
    })
    @PostMapping(value = ApiConstants.ATTACHMENT_FLAG)
    public ResponseEntity<IRunnerResponse> updateConsolidationAttachmentFlag(@RequestParam Boolean enableFlag, @RequestParam Long consolId) {
        consolidationV3Service.updateConsolidationAttachmentFlag(enableFlag, consolId);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {
        @ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.UPDATE_SAILING_SCHEDULE_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.UPDATE_SAILING_SCHEDULE)
    public ResponseEntity<IRunnerResponse> updateSailingScheduleDataToShipment(@RequestBody @Valid ConsolidationSailingScheduleRequest request) throws RunnerException {
        log.info("Received updateSailingSchedule request: {}", request);
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.updateSailingScheduleDataToShipment(request));
    }
}
