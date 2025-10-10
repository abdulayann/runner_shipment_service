package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.DefaultEmailTemplateRequest;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.response.ByteArrayResourceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.ReportExceptionWarning;
import com.dpw.runner.shipment.services.exception.exceptions.TranslationException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReportService;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.UnexpectedRollbackException;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.FETCH_SUCCESSFUL;

@RestController
@RequestMapping(ReportConstants.REPORT_API_HANDLE)
@Slf4j
public class ReportController {

    @Autowired
    private IReportService reportService;

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ReportConstants.REPORT_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ByteArrayResourceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createReport(@RequestBody @Valid ReportRequest request) {
        String responseMsg;
        HttpStatus httpStatus = null;
        try {
            var response = reportService.getDocumentData(CommonRequestModel.buildRequest(request));
            Map<String, Object> documentMap = response.getDocumentServiceMap();
            String fileName = request.getReportInfo() + ".pdf"; // Default filename
            if (documentMap != null && documentMap.get("fileName") != null) {
                fileName = documentMap.get("fileName").toString();   // Override if custom name present in documentMap
            }
            byte[] pdfBytes = response.getContent();
            return ResponseHelper.buildFileResponse(pdfBytes, MediaType.APPLICATION_OCTET_STREAM, fileName, documentMap, "DocMaster");
        } catch (TranslationException e) {
            responseMsg = e.getMessage();
            httpStatus = HttpStatus.PRECONDITION_REQUIRED;
        } catch (UnexpectedRollbackException e) {
            responseMsg = "An error occurred while printing the report. Please contact the support team.";
            log.error(responseMsg, e);
        } catch (ReportExceptionWarning e) {
            log.error(e.getMessage(), e);
            throw e;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg, httpStatus);
    }
    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class)), description = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_CREATE_TAGS_SHIPMENT)
    public ResponseEntity<IRunnerResponse> createDocumentTagsForShipment(@Parameter(description = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @Parameter(description = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid) {
        String responseMsg;
        HttpStatus httpStatus = null;
        try {
            CommonGetRequest request = CommonGetRequest.builder().build();
            id.ifPresent(request::setId);
            guid.ifPresent(request::setGuid);
            return reportService.createDocumentTagsForShipment(CommonRequestModel.buildRequest(request));
        } catch (TranslationException e) {
            responseMsg = e.getMessage();
            httpStatus = HttpStatus.PRECONDITION_REQUIRED;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg, httpStatus);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class)), description = FETCH_SUCCESSFUL)})
    @GetMapping(ReportConstants.PRE_ALERT_EMAIL_TEMPLATE_DATA)
    public ResponseEntity<IRunnerResponse> getPreAlertEmailTemplateData(@RequestParam Long shipmentId, @RequestParam Long emailTemplateId) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(reportService.getPreAlertEmailTemplateData(shipmentId, emailTemplateId));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class)), description = FETCH_SUCCESSFUL)})
    @PostMapping(ReportConstants.EMAIL_TEMPLATE_DATA)
    public ResponseEntity<IRunnerResponse> getEmailTemplateData(@RequestBody DefaultEmailTemplateRequest defaultEmailTemplateRequest) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(reportService.getDefaultEmailTemplateData(defaultEmailTemplateRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = RunnerResponse.class)), description = FETCH_SUCCESSFUL)})
    @PostMapping(ReportConstants.VALIDATE_HOUSE_BILL)
    public ResponseEntity<IRunnerResponse> validateHouseBill(@RequestBody @Valid ReportRequest request) {
        reportService.validateHouseBill(request);
        return ResponseHelper.buildSuccessResponse();
    }
}
