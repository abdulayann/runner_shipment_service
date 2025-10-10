package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.*;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.SEND_SHIPMENT_SUCCESSFUL, content = @Content(schema = @Schema(implementation = SendShipmentResponseClass.class)))
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
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.SEND_CONSOLIDATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = SendConsolidationResponseClass.class)))
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

    @PostMapping(EntityTransferConstants.SEND_ENTITY_TO_EXTERNAL_SYSTEM)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.SEND_SHIPMENT_SUCCESSFUL, content = @Content(schema = @Schema(implementation = SendShipmentResponseClass.class)))
    })
    public ResponseEntity<IRunnerResponse> sendFileToExternalSystem(@RequestBody @Valid SendFileToExternalRequest request) {
        String responseMsg;
        try {
            return entityTransferService.sendFileToExternalSystem(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.IMPORT_SHIPMENT_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ImportShipmentResponse.class)))
    })
    @PostMapping(EntityTransferConstants.IMPORT_SHIPMENT)
    public ResponseEntity<IRunnerResponse> importShipment(@RequestBody ImportShipmentRequest request) {
        String responseMsg;
        try {
            return entityTransferService.importShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.IMPORT_CONSOLIDATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ImportConsolidationResponse.class)))
    })
    @PostMapping(EntityTransferConstants.IMPORT_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> importConsolidation(@RequestBody ImportConsolidationRequest request) {
        String responseMsg;
        try {
            return entityTransferService.importConsolidation(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.SEND_CONSOLIDATION_VALIDATION)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.VALIDATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ValidationResponseClass.class)))
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
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.VALIDATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ValidationResponseClass.class)))
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
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.CHECK_TASK_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CheckTaskExistResponseClass.class)))
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
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.VALIDATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CheckTaskExistResponseClass.class)))
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

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EntityTransferConstants.VALIDATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CheckEntityExistResponse.class)))
    })
    @PostMapping(EntityTransferConstants.CHECK_ENTIRY_EXIST)
    public ResponseEntity<IRunnerResponse> checkEntityExists(@RequestBody @Valid CheckEntityExistRequest request) {
        String responseMsg;
        try {
            return entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(EntityTransferConstants.CHECK_RETRANSFER_ACCEPTED)
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = EntityTransferConstants.RETRANSFER_ACCEPTED, content = @Content(schema = @Schema(implementation = AcceptedFileResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> checkAcceptedFiles(@RequestBody @Valid AcceptedFileRequest request) {
        String responseMsg;
        try {
            return entityTransferService.checkAcceptedFiles(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
