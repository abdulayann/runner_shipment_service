package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.stream.Collectors;

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
    private EmailServiceUtility emailServiceUtility;


    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.shipmentSettingsSync}")
    private String SHIPMENT_SETTING_V1_SYNC_URL;

    @Override
    @Async
    public ResponseEntity<?> sync(ShipmentSettingsDetails req) {
        ShipmentSettingsSyncRequest syncRequest = modelMapper.map(req, ShipmentSettingsSyncRequest.class);

        syncRequest.setHblTermsConditionTemplateRow(convertToList(req.getHblTermsConditionTemplate(), HblTermsConditionTemplateDto.class));
        syncRequest.setHblHawbBackPrintTemplateRow(convertToList(req.getHblHawbBackPrintTemplate(), HblTermsConditionTemplateDto.class));
        syncRequest.setHblLock(convertToList(List.of(req.getHblLockSettings()), HblLockDto.class));
        syncRequest.setHawbLock(convertToList(List.of(req.getHawbLockSettings()), HawbLockDto.class));
        syncRequest.setMawbLock(convertToList(List.of(req.getMawbLockSettings()), MawbLockDto.class));
        syncRequest.setTenantProducts(convertToList(req.getTenantProducts(), TenantProductsDto.class));
        if(req.getProductSequenceConfig() != null) {
            syncRequest.setProductSequenceConfig(req.getProductSequenceConfig().stream()
                    .map(this::mapProductSequenceConfig).toList());
        }

        syncRequest.setShipmentImportApproverRole(req.getShipmentConsoleImportApproverRole());
        syncRequest.setIsLowMarginApprovalRequired(req.getLowMarginApproval());
        syncRequest.setShipmentInstruction(req.getShippingInstruction());
        syncRequest.setISFFileMainPage(req.getIsfFileMainPage());

        String payload = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(syncRequest).module(SyncingConstants.TENANT_SETTINGS).build());
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }
            V1DataSyncResponse response_ = v1Service.v1DataSync(payload);
            if (!response_.getIsSuccess()) {
                try {
                    emailServiceUtility.sendEmailForSyncEntity(String.valueOf(req.getId()), String.valueOf(req.getGuid()),
                            "Shipment Settings Details", response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync failure for Shipment Settings Details: " + ex.getMessage());
                }
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });

        return ResponseHelper.buildSuccessResponse(modelMapper.map(syncRequest, ShipmentSettingsSyncRequest.class));
    }

    @Override
    public ResponseEntity<?> syncSettings() {
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        return sync(shipmentSettingsDetails);
    }

    private ProductSequenceConfigDto mapProductSequenceConfig(ProductSequenceConfig req) {
        if(req == null)
            return null;

        var res = modelMapper.map(req, ProductSequenceConfigDto.class);
        res.setTenantProductObj(modelMapper.map(req.getTenantProducts(), TenantProductsDto.class));

        return res;
    }

    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }
    private  <T,P> P convertToClass(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }
}
