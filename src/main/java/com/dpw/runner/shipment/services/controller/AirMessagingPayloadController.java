package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AirMessagingLogsRequest;
import com.dpw.runner.shipment.services.dto.response.AirMessagingLogsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingPayloadService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.io.IOException;
import java.util.List;


@SuppressWarnings("ALL")
@RestController
@RequestMapping(AirMessagingPayloadConstants.AIR_MESSAGING_PAYLOAD_API_HANDLE)
@Slf4j
public class AirMessagingPayloadController {
    private final IAirMessagingPayloadService airMessagingPayloadService;

    private class MyResponseClass extends RunnerResponse<AirMessagingLogsResponse>{}
    private class MyListResponseClass extends RunnerListResponse<AirMessagingLogsResponse>{}

    @Autowired
    public AirMessagingPayloadController(IAirMessagingPayloadService airMessagingPayloadService) {
        this.airMessagingPayloadService = airMessagingPayloadService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AirMessagingPayloadConstants.AIR_MESSAGING_PAYLOAD_GET_SUCCESSFUL, response = MyResponseClass.class)})
    @GetMapping(ApiConstants.API_GET_FAILED_PAYLOAD)
    public ResponseEntity<JSONObject> getFailedPayloadById(@PathVariable(required = true) Long id) throws RunnerException, IOException {
        return airMessagingPayloadService.getFailedPayloadById(id);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AirMessagingPayloadConstants.AIR_MESSAGING_RESUBMIT_PAYLOAD_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_RESUBMIT_FAILED_PAYLOAD)
    public ResponseEntity<IRunnerResponse> resubmitFailedPayload(@RequestBody Object payload, @RequestParam String messageType) throws RunnerException {
        return airMessagingPayloadService.resubmitFailedPayload(payload, messageType);
    }


}
