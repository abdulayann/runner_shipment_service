package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.AuditLogConstants;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = {AuditLogConstants.AUDIT_LOG_API_HANDLE})
public class AuditLogController {

    private final IAuditLogService auditLogService;

    @Autowired
    public AuditLogController(IAuditLogService auditLogService){
        this.auditLogService = auditLogService;
    }

    private class MyListResponseClass extends RunnerListResponse<AllocationsResponse>{}

    @ApiResponses(value = {@ApiResponse(code = 200, message = AuditLogConstants.AUDIT_LOG_LIST_SUCCESSFUL,
            response = MyListResponseClass.class, responseContainer = AwbConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping("/list")
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return auditLogService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }


    @ApiResponses(value = {@ApiResponse(code = 200, message = AuditLogConstants.AUDIT_LOG_DOWNLOAD_SUCCESSFUL, responseContainer = AwbConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(value = "/download-excel", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<Resource> downloadExcel(@RequestBody @Valid ListCommonRequest listCommonRequest) throws RunnerException {
        Resource resource = auditLogService.downloadExcel(CommonRequestModel.buildRequest(listCommonRequest));
        return ResponseEntity
                .ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=Audit log_" + System.currentTimeMillis() + Constants.XLSX
                ).body(resource);
    }
}
