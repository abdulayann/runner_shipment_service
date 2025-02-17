package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentSettingsPatchRequest;
import com.dpw.runner.shipment.services.dto.request.ProductSequenceConfigRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.request.TemplateUploadRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.syncing.Entity.ProductSequenceConfigDto;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
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

    private final IShipmentSettingsService shipmentSettingsService;
    private final IShipmentSettingsSync shipmentSettingsSync;
    private final IShipmentSettingsReverseSync shipmentSettingsReverseSync;
    private final JsonHelper jsonHelper;

    @Autowired
    public ShipmentSettingsController(IShipmentSettingsService shipmentSettingsService,
                                      IShipmentSettingsSync shipmentSettingsSync,
                                      IShipmentSettingsReverseSync shipmentSettingsReverseSync,
                                      JsonHelper jsonHelper) {
        this.shipmentSettingsService = shipmentSettingsService;
        this.shipmentSettingsSync = shipmentSettingsSync;
        this.shipmentSettingsReverseSync = shipmentSettingsReverseSync;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return shipmentSettingsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE_SETTINGS)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return shipmentSettingsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return shipmentSettingsService.completeUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.SAVE_FROM_V1)
    public ResponseEntity<IRunnerResponse> completeSettingsUpdateCreateV1(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return shipmentSettingsService.completeSettingsUpdateCreateV1(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ShipmentSettingsConstants.SHIPMENT_SETTINGS_ID, required = true) @RequestParam Optional<Long> id, @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_LIST_SUCCESSFUL, responseContainer = ShipmentSettingsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return shipmentSettingsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Optional<Long> id, @RequestParam @Valid Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return shipmentSettingsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_TEMPLATE_UPLOAD_SUCCESSFUL, response = MyListResponseClass.class)})
    @PostMapping(ShipmentSettingsConstants.UPLOAD_TEMPLATE)
    public ResponseEntity<IRunnerResponse> uploadTemplate(@RequestParam MultipartFile file, @RequestParam String previousFileId) {
        TemplateUploadRequest templateUploadRequest = TemplateUploadRequest.builder().file(file).previousFileId(previousFileId).build();
        return shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.TEMPLATE_DOWNLOAD_SUCCESSFUL, response = RunnerListResponse.class)})
    @GetMapping(value = ShipmentSettingsConstants.DOWNLOAD_TEMPLATE, produces = "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    public ResponseEntity<IRunnerResponse> downloadTemplate(@RequestParam String templateId) {
        return shipmentSettingsService.downloadTemplate(templateId);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_TEMPLATE_UPLOAD_SUCCESSFUL, response = RunnerListResponse.class)})
    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ)
    public ResponseEntity<IRunnerResponse> getCustomShipment(@RequestBody @Valid ShipmentSettingRequest request) {
        String responseMsg;
        try {
            return shipmentSettingsSync.sync(
                    jsonHelper.convertValue(request, ShipmentSettingsDetails.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.GET_CUSTOM_PRODUCT_SEQUENCE_SUCCESSFUL, response = MyProductSequenceConfigDtoClass.class)})
    @PostMapping(ApiConstants.API_SYNC_PRODUCT_SEQ)
    public ResponseEntity<IRunnerResponse> getCustomProductSequence(@RequestBody @Valid ProductSequenceConfigRequest request) {
        String responseMsg;
        try {
            return shipmentSettingsSync.syncProductSequence(
                    jsonHelper.convertValue(request, ProductSequenceConfig.class), null);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> syncShipmentToService(@RequestBody @Valid ShipmentSettingsSyncRequest request) {
        String responseMsg = "failure executing :(";
        try {
            return shipmentSettingsReverseSync.reverseSync(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided ShipmentSetting";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_TENANT_ID)
    public ResponseEntity<IRunnerResponse> retrieveByTenantId(@RequestParam Optional<Long> tenantId) {
        CommonGetRequest request = CommonGetRequest.builder().id(tenantId.get()).build();
        String responseMsg;
        try {
            return shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_LIST_COLOAD_STATION_ID)
    public ResponseEntity<IRunnerResponse> listCoLoadStationTenantIds() {
        String responseMsg;
        try {
            return shipmentSettingsService.listCoLoadStationTenantIds();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_LIST_HUBS_STATION_ID)
    public ResponseEntity<IRunnerResponse> listHubTenantIds() {
        String responseMsg;
        try {
            return shipmentSettingsService.listHubTenantIds();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE + ApiConstants.HIDE_MANIFEST)
    public ResponseEntity<IRunnerResponse> updateHideManifest(@RequestParam @Valid boolean hideManifest) {
        String responseMsg;
        try {
            return shipmentSettingsService.hideManifest(hideManifest);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentSettingsConstants.SHIPMENT_SETTINGS_PARTIAL_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PatchMapping(ApiConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<IRunnerResponse> partialUpdate(@RequestBody @Valid ShipmentSettingsPatchRequest shipmentSettingsPatchRequest) {
        String responseMsg;
        try {
            return shipmentSettingsService.partialUpdate(CommonRequestModel.buildRequest(shipmentSettingsPatchRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    private static class MyResponseClass extends RunnerResponse<ShipmentSettingsDetailsResponse> {
    }

    private static class MyListResponseClass extends RunnerListResponse<ShipmentSettingsDetailsResponse> {
    }

    private static class MyProductSequenceConfigDtoClass extends RunnerResponse<ProductSequenceConfigDto> {
    }
}
