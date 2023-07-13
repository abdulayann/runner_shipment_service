package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hpsf.GUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.UUID;

@RestController
@RequestMapping(ShipmentSettingsConstants.SHIPMENT_SETTINGS_API_HANDLE)
@Slf4j
public class ShipmentSettingsController {

    @Autowired
    private IShipmentSettingsService shipmentSettingsService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>> create(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentSettingsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>> retrieveById(@ApiParam(value = ShipmentSettingsConstants.SHIPMENT_SETTINGS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @GetMapping(ApiConstants.API_RETRIEVE_BY_GUID)
    public ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>> retrieveByGuid(@RequestParam String guid) {
        UUID tempGuid = UUID.fromString(guid);
        //CommonGetRequest request = CommonGetRequest.builder().id(uuid).build();
        return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.retrieveByGuid(tempGuid);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_LIST_SUCCESSFUL, responseContainer = ShipmentSettingsConstants.RESPONSE_CONTAINER_LIST) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ShipmentSettingsDetailsResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_DELETE_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) shipmentSettingsService.delete(CommonRequestModel.buildRequest(request));

    }
}
