package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ProductSequenceConfigRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.request.TemplateUploadRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TemplateUploadResponse;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.ProductSequenceConfigDto;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.util.Optional;

@RestController
@RequestMapping(ShipmentSettingsConstants.SHIPMENT_SETTINGS_API_HANDLE)
@Slf4j
public class ShipmentSettingsController {

    @Autowired
    private IShipmentSettingsService shipmentSettingsService;
    @Autowired
    private IShipmentSettingsSync shipmentSettingsSync;
    @Autowired
    private IShipmentSettingsReverseSync shipmentSettingsReverseSync;
    @Autowired
    private JsonHelper jsonHelper;

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
    @PutMapping(ApiConstants.API_UPDATE_SETTINGS)
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

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> completeUpdate(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentSettingsService.completeUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.SAVE_FROM_V1)
    public ResponseEntity<RunnerResponse> completeSettingsUpdateCreateV1(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentSettingsService.completeSettingsUpdateCreateV1(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>> retrieveById(@ApiParam(value = ShipmentSettingsConstants.SHIPMENT_SETTINGS_ID, required = true) @RequestParam Optional<Long> id, @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_LIST_SUCCESSFUL, responseContainer = ShipmentSettingsConstants.RESPONSE_CONTAINER_LIST) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ShipmentSettingsDetailsResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_DELETE_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Optional<Long> id, @RequestParam @Valid Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return (ResponseEntity<RunnerResponse>) shipmentSettingsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_TEMPLATE_UPLOAD_SUCCESSFUL) })
    @PostMapping(ShipmentSettingsConstants.UPLOAD_TEMPLATE)
    public ResponseEntity<RunnerListResponse<TemplateUploadResponse>> uploadTemplate(@RequestParam MultipartFile file, @RequestParam String previousFileId) {
        TemplateUploadRequest templateUploadRequest = TemplateUploadRequest.builder().file(file).previousFileId(previousFileId).build();
        return (ResponseEntity<RunnerListResponse<TemplateUploadResponse>>) shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.TEMPLATE_DOWNLOAD_SUCCESSFUL) })
    @GetMapping(value = ShipmentSettingsConstants.DOWNLOAD_TEMPLATE, produces = "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    public ResponseEntity<?> downloadTemplate(@RequestParam String templateId) {
        return (ResponseEntity<?>) shipmentSettingsService.downloadTemplate(templateId);
    }

    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ)
    public ResponseEntity<RunnerResponse<CustomShipmentSyncRequest>> getCustomShipment(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<CustomShipmentSyncRequest>>) shipmentSettingsSync.sync(
                    jsonHelper.convertValue(request, ShipmentSettingsDetails.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<CustomShipmentSyncRequest>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(ApiConstants.API_SYNC_PRODUCT_SEQ)
    public ResponseEntity<RunnerResponse<ProductSequenceConfigDto>> getCustomProductSequence(@RequestBody @Valid ProductSequenceConfigRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ProductSequenceConfigDto>>) shipmentSettingsSync.syncProductSequence(
                    jsonHelper.convertValue(request, ProductSequenceConfig.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ProductSequenceConfigDto>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    public ResponseEntity<?> syncShipmentToService(@RequestBody @Valid ShipmentSettingsSyncRequest request){
        String responseMsg = "failure executing :(";
        try {
            return shipmentSettingsReverseSync.reverseSync(CommonRequestModel.buildRequest(request));
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided ShipmentSetting";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_TENANT_ID)
    public ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>> retrieveByTenantId(@RequestParam Optional<Long> tenantId) {
        CommonGetRequest request = CommonGetRequest.builder().id(tenantId.get()).build();
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ShipmentSettingsDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
