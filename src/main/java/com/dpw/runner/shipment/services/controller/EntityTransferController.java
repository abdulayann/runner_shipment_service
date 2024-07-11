package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsolidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
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

    private final IEntityTransferService entityTransferService;
    private static class CheckTaskExistResponseClass extends RunnerResponse<CheckTaskExistResponse>{}
    private static class ValidationResponseClass extends RunnerResponse<ValidationResponse>{}
    private static class SendConsolidationResponseClass extends RunnerResponse<SendConsolidationResponse>{}
    private static class SendShipmentResponseClass extends RunnerResponse<SendShipmentResponse>{}

    @Autowired
    public EntityTransferController(IEntityTransferService entityTransferService) {
        this.entityTransferService = entityTransferService;
    }

    @PostMapping(EntityTransferConstants.SEND_SHIPMENT)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.SEND_SHIPMENT_SUCCESSFUL, response = SendShipmentResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> sendShipment(@RequestBody @Valid SendShipmentRequest request) {
        String responseMsg;
        try {
            return entityTransferService.sendShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.SEND_CONSOLIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.SEND_CONSOLIDATION_SUCCESSFUL, response = SendConsolidationResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> sendConsolidation(@RequestBody @Valid @NonNull SendConsolidationRequest request) {
        String responseMsg;
        try {
            return entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

//    @PostMapping(EntityTransferConstants.IMPORT_SHIPMENT)
//    @ApiResponses(value = {
//            @ApiResponse(code = 200, message = EntityTransferConstants.IMPORT_SHIPMENT_SUCCESSFUL, response = ImportShipmentResponseClass.class)
//    })
//    public ResponseEntity<IRunnerResponse> importShipment(@RequestBody @Valid ImportShipmentRequest request) {
//        String responseMsg;
//        ImportShipmentRequest importShipmentRequest = jsonHelper.convertValue(request, ImportShipmentRequest.class);
//        try {
//            return entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
//            log.error(responseMsg, e);
//        }
//        return ResponseHelper.buildFailedResponse(responseMsg);
//    }
//
//    @PostMapping(EntityTransferConstants.IMPORT_CONSOLIDATION)
//    @ApiResponses(value = {
//            @ApiResponse(code = 200, message = EntityTransferConstants.IMPORT_CONSOLIDATION_SUCCESSFUL, response = ImportConsolidationResponseClass.class)
//    })
//    public ResponseEntity<IRunnerResponse> importConsolidation(@RequestBody @Valid ImportConsolidationRequest request) {
//        String responseMsg;
//        ImportConsolidationRequest importConsolidationRequest = jsonHelper.convertValue(request, ImportConsolidationRequest.class);
//        try {
//            return entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
//            log.error(responseMsg, e);
//        }
//        return ResponseHelper.buildFailedResponse(responseMsg);
//    }

    @PostMapping(EntityTransferConstants.SEND_CONSOLIDATION_VALIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.VALIDATION_SUCCESSFUL, response = ValidationResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> sendConsolidationValidation(@RequestBody @Valid ValidateSendConsolidationRequest request) {
        String responseMsg;
        try {
            return entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.SEND_SHIPMENT_VALIDATION)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.VALIDATION_SUCCESSFUL, response = ValidationResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> sendShipmentValidation(@RequestBody @Valid ValidateSendShipmentRequest request) {
        String responseMsg;
        try {
            return entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }


    @PostMapping(EntityTransferConstants.CHECK_TASK_EXIST)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.CHECK_TASK_SUCCESSFUL, response = CheckTaskExistResponseClass.class)
    })
    public ResponseEntity<IRunnerResponse> checkTaskExist(@RequestBody @Valid CheckTaskExistRequest request) {
        String responseMsg;
        try {
            return entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EntityTransferConstants.VALIDATION_SUCCESSFUL, response = CheckTaskExistResponseClass.class)
    })
    @PostMapping(EntityTransferConstants.POST_AR_VALIDATION)
    public ResponseEntity<IRunnerResponse> postArValidation(@RequestBody @Valid PostArValidationRequest request) {
        String responseMsg;
        HttpStatus httpStatus = null;
        try {
            return entityTransferService.postArValidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg, httpStatus);
    }



}
