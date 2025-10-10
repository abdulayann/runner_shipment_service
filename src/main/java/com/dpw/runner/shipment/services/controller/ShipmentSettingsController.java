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
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

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
    private static class MyResponseClass extends RunnerResponse<ShipmentSettingsDetailsResponse>{}
    private static class MyListResponseClass extends RunnerListResponse<ShipmentSettingsDetailsResponse>{}


    private static class MyProductSequenceConfigDtoClass extends RunnerResponse<ProductSequenceConfigDto>{}

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_ID, required = true) @RequestParam Optional<Long> id, @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return shipmentSettingsService.retrieveById(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = { @ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = MyListResponseClass.class))), description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_LIST_SUCCESSFUL) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return shipmentSettingsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Optional<Long> id, @RequestParam @Valid Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return shipmentSettingsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_TEMPLATE_UPLOAD_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyListResponseClass.class)))})
    @PostMapping(ShipmentSettingsConstants.UPLOAD_TEMPLATE)
    public ResponseEntity<IRunnerResponse> uploadTemplate(@RequestParam MultipartFile file, @RequestParam String previousFileId) {
        TemplateUploadRequest templateUploadRequest = TemplateUploadRequest.builder().file(file).previousFileId(previousFileId).build();
        return shipmentSettingsService.uploadTemplate(CommonRequestModel.buildRequest(templateUploadRequest));
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.TEMPLATE_DOWNLOAD_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerListResponse.class)))})
    @GetMapping(value = ShipmentSettingsConstants.DOWNLOAD_TEMPLATE, produces = "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    public ResponseEntity<IRunnerResponse> downloadTemplate(@RequestParam String templateId) {
        return shipmentSettingsService.downloadTemplate(templateId);
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_TEMPLATE_UPLOAD_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerListResponse.class)))})
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.GET_CUSTOM_PRODUCT_SEQUENCE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyProductSequenceConfigDtoClass.class)))})
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
            @ApiResponse(responseCode = "200", description = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.SYNC)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> syncShipmentToService(@RequestBody @Valid ShipmentSettingsSyncRequest request){
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_TENANT_ID)
    public ResponseEntity<IRunnerResponse> retrieveByTenantId(@RequestParam Optional<Long> tenantId, @RequestParam(required = false, defaultValue = "false") Boolean sectionRule) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(tenantId.get()).sectionRule(sectionRule).build();
            return shipmentSettingsService.retrieveByTenantId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL) })
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class)), description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_SUCCESSFUL) })
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
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

    @ApiResponses(value = { @ApiResponse(responseCode = "200", description = ShipmentSettingsConstants.SHIPMENT_SETTINGS_PARTIAL_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))})
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
}
