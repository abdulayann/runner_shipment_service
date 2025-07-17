package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportV3ConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportV3ShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ImportConsolidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ImportShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsolidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferV3Service;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.fasterxml.jackson.databind.JsonMappingException;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping(EntityTransferConstants.ENTITY_TRANSFER_V3_API_HANDLE)
@Slf4j
public class EntityTransferV3Controller {

    private final IEntityTransferV3Service entityTransferService;
    private static class SendConsolidationResponseClass extends RunnerResponse<SendConsolidationResponse>{}
    private static class SendShipmentResponseClass extends RunnerResponse<SendShipmentResponse>{}
    private final JsonHelper jsonHelper;

    @Autowired
    public EntityTransferV3Controller(IEntityTransferV3Service entityTransferService, JsonHelper jsonHelper) {
        this.entityTransferService = entityTransferService;
        this.jsonHelper = jsonHelper;

    }

    @PostMapping(EntityTransferConstants.SEND_SHIPMENT)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.SEND_SHIPMENT_SUCCESSFUL, response = SendShipmentResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> sendShipment(@RequestBody @Valid SendShipmentRequest request) throws RunnerException {
        log.info("Received Send Shipment Request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        List<Integer> successTenantIds = entityTransferService.sendShipment(CommonRequestModel.buildRequest(request));
        List<String> tenantName = entityTransferService.getTenantName(successTenantIds);
        SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(successTenantIds)
                .message(String.format("Shipment Sent to branches %s", String.join(", ", tenantName)))
                .build();
        return ResponseHelper.buildSuccessResponse(sendShipmentResponse);
    }

    @PostMapping(EntityTransferConstants.SEND_CONSOLIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.SEND_CONSOLIDATION_SUCCESSFUL, response = SendConsolidationResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> sendConsolidation(@RequestBody @Valid @NonNull SendConsolidationRequest request) throws RunnerException {
        log.info("Received Send Consolidation Request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        List<Integer> successTenantIds = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(request));
        List<String> tenantName = entityTransferService.getTenantName(successTenantIds);
        SendConsolidationResponse sendConsolidationResponse = SendConsolidationResponse.builder().successTenantIds(successTenantIds)
                .message(String.format("Consolidation Sent to branches %s", String.join(", ", tenantName)))
                .build();
        return ResponseHelper.buildSuccessResponse(sendConsolidationResponse);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.IMPORT_SHIPMENT_SUCCESSFUL, response = ImportShipmentResponse.class)
    })
    @PostMapping(EntityTransferConstants.IMPORT_SHIPMENT)
    public ResponseEntity<IRunnerResponse> importShipment(@RequestBody ImportV3ShipmentRequest request) throws RunnerException, JsonMappingException {
        log.info("Received Import Shipment Request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String shipmentId =  entityTransferService.importShipment(CommonRequestModel.buildRequest(request));
        if(shipmentId==null)
            ResponseHelper.buildSuccessResponse();
        var response = ImportShipmentResponse.builder()
                .shipmentId(shipmentId)
                .message("Shipment Imported Successfully with Shipment Number: " + shipmentId)
                .build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.IMPORT_CONSOLIDATION_SUCCESSFUL, response = ImportConsolidationResponse.class)
    })
    @PostMapping(EntityTransferConstants.IMPORT_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> importConsolidation(@RequestBody ImportV3ConsolidationRequest request) {
        String responseMsg;
        try {
            log.info("Received Import Consolidation Request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            return entityTransferService.importConsolidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
