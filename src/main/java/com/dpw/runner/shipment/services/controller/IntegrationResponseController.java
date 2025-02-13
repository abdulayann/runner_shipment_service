package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.IntegrationResponseConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IIntegrationResponseService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = IntegrationResponseConstants.INTEGRATION_RESPONSE_API_HANDLE)
public class IntegrationResponseController {
    private final IIntegrationResponseService integrationResponseService;

    @Autowired
    public IntegrationResponseController(IIntegrationResponseService integrationResponseService) {
        this.integrationResponseService = integrationResponseService;
    }

    @PostMapping(IntegrationResponseConstants.FETCH_INTEGRATION_RESPONSES)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = IntegrationResponseConstants.FETCH_RESPONSES_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })

    public ResponseEntity<IRunnerResponse> fetchIntegrationResponses(@RequestBody @Valid IntegrationResponseRequest request) {
        String responseMsg;
        try {
            return integrationResponseService.fetchIntegrationResponses(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : IntegrationResponseConstants.RESPONSE_FETCH_FAILED;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
