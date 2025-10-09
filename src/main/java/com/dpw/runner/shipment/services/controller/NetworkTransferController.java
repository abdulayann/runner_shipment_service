package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.annotations.RequireApiKey;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.NetworkTransferRequest;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferListResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;
import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = NetworkTransferConstants.NETWORK_TRANSFER_API_HANDLE)
public class NetworkTransferController {
    private INetworkTransferService networkTransferService;

    private static class MyResponseClass extends RunnerResponse<NetworkTransferResponse> {}
    private static class MyListResponseClass extends RunnerListResponse<NetworkTransferListResponse> {}

    @Autowired
    public NetworkTransferController(INetworkTransferService networkTransferService){
        this.networkTransferService = networkTransferService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyListResponseClass.class, message = NetworkTransferConstants.LIST_SUCCESSFUL, responseContainer = NetworkTransferConstants.RESPONSE_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    @PreAuthorize("hasAuthority('" + PermissionConstants.SHIPMENT_IN_PIPELINE_VIEW + "')")
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return networkTransferService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyResponseClass.class, message = NetworkTransferConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    @PreAuthorize("hasAuthority('" + PermissionConstants.SHIPMENT_IN_PIPELINE_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = NetworkTransferConstants.NETWORK_TRANSFER_ID) @RequestParam Optional<Long> id, @ApiParam(value = NetworkTransferConstants.NETWORK_TRANSFER_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return networkTransferService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyResponseClass.class, message = NetworkTransferConstants.REQUEST_FOR_TRANSFER_SUCCESSFUL)})
    @PostMapping(NetworkTransferConstants.NETWORK_REQUEST_FOR_TRANSFER)
    @PreAuthorize("hasAuthority('" + PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> requestForTransfer(@RequestBody @Valid RequestForTransferRequest requestForTransferRequest) {
        String responseMsg;
        try {
            return networkTransferService.requestForTransfer(CommonRequestModel.buildRequest(requestForTransferRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyResponseClass.class, message = NetworkTransferConstants.REQUEST_FOR_REASSIGNED_SUCCESSFUL)})
    @PostMapping(NetworkTransferConstants.NETWORK_REASSIGNED)
    @PreAuthorize("hasAuthority('" + PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> requestForReassign(@RequestBody @Valid ReassignRequest reassignRequest) {
        String responseMsg;
        try {
            return networkTransferService.requestForReassign(CommonRequestModel.buildRequest(reassignRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NetworkTransferController.MyResponseClass.class, message = NetworkTransferConstants.FETCH_ENTITY_STATUS_SUCCESSFUL)})
    @GetMapping(NetworkTransferConstants.SHIPMENT_ENTITY_STATUS)
    public ResponseEntity<IRunnerResponse> fetchEntityStatus(@ApiParam(value = NetworkTransferConstants.ENTITY_GUID) @RequestParam(required = true) String guid) {
        CommonGetRequest request = CommonGetRequest.builder().guid(guid).build();
        return networkTransferService.fetchEntityStatus(request);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(NetworkTransferConstants.NETWORK_TRANSFER_CREATE_EXTERNAL)
    public ResponseEntity<IRunnerResponse> createExternal(@RequestBody @Valid NetworkTransferRequest networkTransferRequest) {
        String responseMessage;
        try {
            return networkTransferService.createExternal(CommonRequestModel.buildRequest(networkTransferRequest));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }
        return ResponseHelper.buildFailedResponse(responseMessage);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @RequireApiKey
    @ExcludeTimeZone
    @PostMapping(NetworkTransferConstants.NETWORK_TRANSFER_CREATE_EXTERNAL_BRIDGE)
    public ResponseEntity<IRunnerResponse> createExternalViaBridge(@RequestBody @Valid NetworkTransferRequest networkTransferRequest) {
        String responseMessage;
        try {
            TenantContext.setCurrentTenant(networkTransferRequest.getTenantId());
            UsersDto usersDto = new UsersDto();
            usersDto.setTenantId(networkTransferRequest.getTenantId());
            UserContext.setUser(usersDto);
            return networkTransferService.createExternal(CommonRequestModel.buildRequest(networkTransferRequest));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
        }
        return ResponseHelper.buildFailedResponse(responseMessage);
    }
}
