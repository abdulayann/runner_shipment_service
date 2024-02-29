package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TriggerSyncRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(SyncQueueConstants.SYNC_API_HANDLE)
@Slf4j
public class SyncQueueController {

    private final ISyncQueueService syncQueueService;

    @Autowired
    public SyncQueueController(ISyncQueueService syncQueueService){
        this.syncQueueService = syncQueueService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = SyncQueueConstants.GENERATION_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(SyncQueueConstants.TRIGGER_SYNC)
    @PreAuthorize("hasAuthority('" + PermissionConstants.tenantSuperAdmin + "')")
    public ResponseEntity<IRunnerResponse> triggerSyncRequest(@RequestBody @Valid TriggerSyncRequest request) {
        String responseMsg;
        try {
            return syncQueueService.triggerSyncRequest(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
