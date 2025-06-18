package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.List;

@Service
@Slf4j
public class ShipmentSettingsSync implements IShipmentSettingsSync {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    RestTemplate restTemplate;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private V1AuthHelper v1AuthHelper;

    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private ISyncService syncService;

    @Value("${v1service.url.base}${v1service.url.shipmentSettingsSync}")
    private String SHIPMENT_SETTING_V1_SYNC_URL;

    @Override
    public ResponseEntity<IRunnerResponse> sync(ShipmentSettingsDetails req) {
        ShipmentSettingsSyncRequest syncRequest = modelMapper.map(req, ShipmentSettingsSyncRequest.class);

        syncRequest.setHblTermsConditionTemplateRow(convertToList(req.getHblTermsConditionTemplate(), HblTermsConditionTemplateDto.class));
        syncRequest.setHblHawbBackPrintTemplateRow(convertToList(req.getHblHawbBackPrintTemplate(), HblTermsConditionTemplateDto.class));
        syncRequest.setHblLock(convertToList(List.of(req.getHblLockSettings()), HblLockDto.class));
        syncRequest.setHawbLock(convertToList(List.of(req.getHawbLockSettings()), HawbLockDto.class));
        syncRequest.setMawbLock(convertToList(List.of(req.getMawbLockSettings()), MawbLockDto.class));

        syncRequest.setShipmentImportApproverRole(req.getShipmentConsoleImportApproverRole());
        syncRequest.setIsLowMarginApprovalRequired(req.getLowMarginApproval());
        syncRequest.setShipmentInstruction(req.getShippingInstruction());
        syncRequest.setISFFileMainPage(req.getIsfFileMainPage());
        syncRequest.setAirExportConsolManifest(req.getAirExportConsoleManifest());
        syncRequest.setAirImportConsolManifest(req.getAirImportConsoleManifest());
        syncRequest.setSeaImportConsolManifest(req.getSeaImportConsoleManifest());
        syncRequest.setSeaExportConsolManifest(req.getSeaExportConsoleManifest());
        syncRequest.setShipmentServiceV3Enabled(req.getIsRunnerV3Enabled());

        String payload = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(syncRequest).module(SyncingConstants.TENANT_SETTINGS).build());
        syncService.pushToKafka(payload,String.valueOf(req.getId()), String.valueOf(req.getGuid()), SyncingConstants.TENANT_SETTINGS, String.valueOf(req.getGuid()));
        return ResponseHelper.buildSuccessResponse(modelMapper.map(syncRequest, ShipmentSettingsSyncRequest.class));
    }

    public ResponseEntity<IRunnerResponse> syncProductSequence(ProductSequenceConfig productSequenceConfig, HttpHeaders headers) throws RunnerException {
        ProductSequenceConfigDto productSequenceConfigDto = mapProductSequenceConfig(productSequenceConfig);
        String payload = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(productSequenceConfigDto).module(SyncingConstants.PRODUCT_SEQUENCE).build());
        String guid = StringUtility.convertToString(productSequenceConfig.getGuid());
        syncService.pushToKafka(payload, StringUtility.convertToString(productSequenceConfig.getId()), guid, "Product Sequence", guid);
        return ResponseHelper.buildSuccessResponse(productSequenceConfigDto);
    }

    @Override
    public ResponseEntity<IRunnerResponse> syncSettings() {
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        return sync(shipmentSettingsDetails);
    }

    private ProductSequenceConfigDto mapProductSequenceConfig(ProductSequenceConfig req) {

        var res = modelMapper.map(req, ProductSequenceConfigDto.class);
        res.setTenantProductObj(modelMapper.map(req.getTenantProducts(), TenantProductsDto.class));

        return res;
    }

    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }
    private  <T,P> P convertToClass(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }
}
