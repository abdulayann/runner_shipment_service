package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dto.request.ProductSequenceConfigRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
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

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.shipmentSettingsSync}")
    private String SHIPMENT_SETTING_V1_SYNC_URL;

    @Override
    public ResponseEntity<?> sync(ShipmentSettingRequest req) {
        ShipmentSettingsSyncRequest syncRequest = modelMapper.map(req, ShipmentSettingsSyncRequest.class);

        syncRequest.setHblTermsConditionTemplateRow(convertToList(req.getHblTermsConditionTemplate(), HblTermsConditionTemplateDto.class));
        syncRequest.setHblHawbBackPrintTemplateRow(convertToList(req.getHblHawbBackPrintTemplate(), HblTermsConditionTemplateDto.class));
        syncRequest.setHblLock(convertToList(List.of(req.getHblLockSettings()), HblLockDto.class));
        syncRequest.setTenantProducts(convertToList(req.getTenantProducts(), TenantProductsDto.class));
        if(req.getProductSequenceConfig() != null) {
            syncRequest.setProductSequenceConfig(req.getProductSequenceConfig().stream()
                    .map(this::mapProductSequenceConfig).toList());
        }

        syncRequest.setShipmentImportApproverRole(req.getShipmentConsoleImportApproverRole());
        syncRequest.setIsLowMarginApprovalRequired(req.getLowMarginApproval());
        syncRequest.setShipmentInstruction(req.getShippingInstruction());

        String payload = jsonHelper.convertToJson(syncRequest);
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            HttpEntity<V1DataResponse> entity = new HttpEntity(payload, V1AuthHelper.getHeaders());
            var response = this.restTemplate.postForEntity(this.SHIPMENT_SETTING_V1_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
            return response;
        });

        return ResponseHelper.buildSuccessResponse(modelMapper.map(syncRequest, ShipmentSettingsSyncRequest.class));
    }

    private ProductSequenceConfigDto mapProductSequenceConfig(ProductSequenceConfigRequest req) {
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
