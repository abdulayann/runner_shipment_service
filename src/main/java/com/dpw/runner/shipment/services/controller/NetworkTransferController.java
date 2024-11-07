package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.NetworkTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TransferredNetworkTransferRequest;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = NetworkTransferConstants.NETWORK_TRANSFER_API_HANDLE)
public class NetworkTransferController {
    private INetworkTransferService networkTransferService;

    private class MyResponseClass extends RunnerResponse<NetworkTransferResponse> {}
    private class MyListResponseClass extends RunnerListResponse<NetworkTransferResponse> {}

    @Autowired
    public NetworkTransferController(ICRPServiceAdapter crpService, JsonHelper jsonHelper, INetworkTransferService networkTransferService){
        this.networkTransferService = networkTransferService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyListResponseClass.class, message = NetworkTransferConstants.LIST_SUCCESSFUL, responseContainer = NetworkTransferConstants.RESPONSE_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return networkTransferService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyResponseClass.class, message = NetworkTransferConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = NetworkTransferConstants.NETWORK_TRANSFER_ID) @RequestParam Optional<Long> id, @ApiParam(value = NetworkTransferConstants.NETWORK_TRANSFER_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return networkTransferService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @PutMapping(ApiConstants.UPDATE_NT_TRANSFERRED_STATUS)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = NetworkTransferConstants.UPDATE_NT_TRANSFERRED_STATUS),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> updateNtTransferred(@RequestBody @Valid TransferredNetworkTransferRequest request) {
        log.info("Request received for updating the network transfer");
        try {
            return networkTransferService.transferredNetworkTransferStatus(request);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }
}
