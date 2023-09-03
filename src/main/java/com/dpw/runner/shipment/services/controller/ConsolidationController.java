package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.concurrent.ExecutionException;

@RestController
@RequestMapping(ConsolidationConstants.CONSOLIDATION_API_HANDLE)
@Slf4j
public class ConsolidationController {

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    JsonHelper jsonHelper;

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Consolidation Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/list-consolidation")
    public ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>> fetchByQuery(@RequestBody @NonNull ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>>) consolidationService.fetchConsolidations(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @PostMapping(value = "/create-test-consolidation/{count}")
    public ResponseEntity<?> createTestRecord(@PathVariable Integer count) {
        ResponseEntity<List<ConsolidationDetails>> response = ResponseEntity.status(HttpStatus.OK)
                .body(consolidationService.createTestConsolidations(count));
        return response;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> create(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            ConsolidationDetailsRequest req = jsonHelper.convertValue(request, ConsolidationDetailsRequest.class);
            return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.create(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) consolidationService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.LIST_SUCCESSFUL, responseContainer = ConsolidationConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>>) consolidationService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> retrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> completeRetrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE_CONSOLIDATION)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) consolidationService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> completeUpdate(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) consolidationService.completeUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.LOCK_TOGGLE_SUCCESSFUL)})
    @GetMapping(ApiConstants.TOGGLE_LOCK)
    public ResponseEntity<RunnerResponse> toggleLock(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) consolidationService.toggleLock(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_UTILIZATION)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> calculateUtilization(@RequestBody ConsolidationDetailsRequest consolidationDetailsRequest) {
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.calculateUtilization(CommonRequestModel.buildRequest(consolidationDetailsRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CHANGE_UNIT_ALLOCATED_ACHIEVED)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> calculateAchieved_AllocatedForSameUnit(@RequestBody ConsolidationDetailsRequest consolidationDetailsRequest) {
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(consolidationDetailsRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_CHARGEABLE)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> calculateChargeable(@RequestBody ConsolidationDetailsRequest consolidationDetailsRequest) {
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.calculateChargeable(CommonRequestModel.buildRequest(consolidationDetailsRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_VALUES)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> calculateAchievedValues(@RequestBody ConsolidationDetailsRequest consolidationDetailsRequest) {
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.calculateAchievedValues(CommonRequestModel.buildRequest(consolidationDetailsRequest));
    }

    @PostMapping(ApiConstants.ATTACH_SHIPMENTS)
    public ResponseEntity<RunnerResponse> attachShipments(@RequestBody @Valid ShipmentAttachDetachRequest request) throws Exception {

        return (ResponseEntity<RunnerResponse>) consolidationService.attachShipments(request.getId(), request.getShipmentIds());
    }

    @PostMapping(ApiConstants.DETACH_SHIPMENTS)
    public ResponseEntity<RunnerResponse> detachShipments(@RequestBody @Valid ShipmentAttachDetachRequest request) throws Exception{
        return (ResponseEntity<RunnerResponse>) consolidationService.detachShipments(request.getId(), request.getShipmentIds());
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ConsolidationConstants.CONSOLIDATION_V1_CREATE)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> createV1Consolidation(@RequestBody @Valid CustomConsolidationRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationSync.reverseSync(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ)
    public ResponseEntity<RunnerResponse<CustomConsolidationRequest>> getCustomConsol(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<CustomConsolidationRequest>>) consolidationSync.sync(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<CustomConsolidationRequest>>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
