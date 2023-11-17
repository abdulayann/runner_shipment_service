package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
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

@RestController
@RequestMapping(EntityTransferConstants.ENTITY_TRANSFER_API_HANDLE)
@Slf4j
public class EntityTransferController {

    @Autowired
    private IEntityTransferService entityTransferService;
    @Autowired
    JsonHelper jsonHelper;

    @PostMapping(EntityTransferConstants.SEND_SHIPMENT)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.SEND_SHIPMENT_SUCCESSFUL)
    })
    public ResponseEntity<?> sendShipment(@RequestBody @Valid SendShipmentRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) entityTransferService.sendShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.SEND_CONSOLIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.SEND_CONSOLIDATION_SUCCESSFUL)
    })
    public ResponseEntity<?> sendConsolidation(@RequestBody @Valid @NonNull SendConsolidationRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.IMPORT_SHIPMENT)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.IMPORT_SHIPMENT_SUCCESSFUL)
    })
    public ResponseEntity<?> importShipment(@RequestBody @Valid ImportShipmentRequest request) {
        String responseMsg;
        ImportShipmentRequest importShipmentRequest = jsonHelper.convertValue(request, ImportShipmentRequest.class);
        try {
            return (ResponseEntity<RunnerResponse>) entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.IMPORT_CONSOLIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.IMPORT_CONSOLIDATION_SUCCESSFUL)
    })
    public ResponseEntity<?> importConsolidation(@RequestBody @Valid ImportConsolidationRequest request) {
        String responseMsg;
        ImportConsolidationRequest importConsolidationRequest = jsonHelper.convertValue(request, ImportConsolidationRequest.class);
        try {
            return (ResponseEntity<RunnerResponse>) entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.SEND_CONSOLIDATION_VALIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.VALIDATION_SUCCESSFUL)
    })
    public ResponseEntity<?> sendConsolidationValidation(@RequestBody @Valid ValidateSendConsolidationRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.SEND_SHIPMENT_VALIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.VALIDATION_SUCCESSFUL)
    })
    public ResponseEntity<?> sendShipmentValidation(@RequestBody @Valid ValidateSendShipmentRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }


    @PostMapping(EntityTransferConstants.CHECK_TASK_EXIST)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.CHECK_TASK_SUCCESSFUL)
    })
    public ResponseEntity<?> checkTaskExist(@RequestBody @Valid CheckTaskExistRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<CheckTaskExistResponse>) entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }



}
