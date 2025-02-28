package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.response.ByteArrayResourceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.TranslationException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReportService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

@RestController
@RequestMapping(ReportConstants.REPORT_API_HANDLE)
@Slf4j
public class ReportController {

    @Autowired
    private IReportService reportService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ReportConstants.REPORT_CREATE_SUCCESSFUL, response = ByteArrayResourceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createReport(@RequestBody @Valid ReportRequest request) {
        String responseMsg;
        HttpStatus httpStatus = null;
        try {
            return ResponseHelper.buildFileResponse(reportService.getDocumentData(CommonRequestModel.buildRequest(request)), MediaType.APPLICATION_OCTET_STREAM, request.getReportInfo() + ".pdf");
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

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_CREATE_TAGS_SHIPMENT)
    public ResponseEntity<IRunnerResponse> createDocumentTagsForShipment(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid) {
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
}
