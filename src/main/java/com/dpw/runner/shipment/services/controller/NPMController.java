package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INPMService;
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
@RequestMapping(value = NPMConstants.NPM_API_HANDLE)
public class NPMController {
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private INPMService npmService;
    @PostMapping(NPMConstants.LIST_CONTRACT)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = NPMConstants.CONTRACT_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<?> fetchContracts(@RequestBody @Valid ListContractRequest request) {
        String responseMsg;
        ListContractRequest listContractRequest = jsonHelper.convertValue(request, ListContractRequest.class);
        try {
            return (ResponseEntity<RunnerResponse>) npmService.fetchContracts(CommonRequestModel.buildRequest(listContractRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : NPMConstants.CONTRACT_LIST_FAILED;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
