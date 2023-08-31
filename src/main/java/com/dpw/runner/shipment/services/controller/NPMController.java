package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = NPMConstants.NPM_API_HANDLE)
public class NPMController {
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private INPMServiceAdapter npmService;

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

    @PostMapping(NPMConstants.GET_OFFERS)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = NPMConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<?> getNPMOffers(@RequestBody @Valid NPMFetchOffersRequestFromUI request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) npmService.fetchOffers(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.GET_OFFERS_V8)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = NPMConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<?> getNPMOffersV8(@RequestBody @Valid NPMFetchOffersRequestFromUI request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) npmService.fetchOffersV8(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
