package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ShipmentSettingsReverseSync implements IShipmentReverseSync {
    @Autowired
    IShipmentSettingsService shipmentSettingsService;
    @Autowired
    ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> reverseSync(ShipmentSettingsSyncRequest req) {
        String responseMessage;
        try {
            ShipmentSettingRequest dest = modelMapper.map(req, ShipmentSettingRequest.class);

            dest.setHblTermsConditionTemplate(convertToList(req.getHblTermsConditionTemplateRow(), HblTermsConditionTemplateRequest.class));
            dest.setHblHawbBackPrintTemplate(convertToList(req.getHblHawbBackPrintTemplateRow(), HblTermsConditionTemplateRequest.class));
            dest.setHblLockSettings(convertToClass(req.getHblLock(), HblLockSettingsRequest.class));
            dest.setTenantProducts(convertToList(req.getTenantProducts(), TenantProductsRequest.class));
            dest.setProductSequenceConfig(convertToList(req.getProductSequenceConfig(), ProductSequenceConfigRequest.class));
            dest.setHawbLockSettings(convertToClass(req.getHawbLock(), HawbLockSettingsRequest.class));
            dest.setMawbLockSettings(convertToClass(req.getMawb(), MawbLockSettingsRequest.class));

            return shipmentSettingsService.completeSettingsUpdateCreateV1(CommonRequestModel.buildRequest(dest));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
            return ResponseHelper.buildFailedResponse(responseMessage);
        }
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
