package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import com.dpw.runner.shipment.services.entity.enums.TypeOfHblPrint;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.syncing.Entity.HblTermsConditionTemplateDto;
import com.dpw.runner.shipment.services.syncing.Entity.ProductSequenceConfigDto;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.TenantProductsDto;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsReverseSync;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ShipmentSettingsReverseSync implements IShipmentSettingsReverseSync {
    @Autowired
    IShipmentSettingsService shipmentSettingsService;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    IShipmentSettingsDao shipmentSettingsDao;

    @Override
    public ResponseEntity<?> reverseSync(CommonRequestModel commonRequestModel) {
        String responseMessage;
        ShipmentSettingsSyncRequest req = (ShipmentSettingsSyncRequest) commonRequestModel.getData();
        try {
            ShipmentSettingRequest dest = modelMapper.map(req, ShipmentSettingRequest.class);

            // Non Enums entities
            if(req.getHblLock() != null && req.getHblLock().size() > 0)
                dest.setHblLockSettings(convertToClass(req.getHblLock().get(0), HblLockSettingsRequest.class));
            if(req.getHawbLock() != null && req.getHawbLock().size() > 0)
                dest.setHawbLockSettings(convertToClass(req.getHawbLock().get(0), HawbLockSettingsRequest.class));
            if(req.getMawbLock() != null && req.getMawbLock().size() > 0)
                dest.setMawbLockSettings(convertToClass(req.getMawbLock().get(0), MawbLockSettingsRequest.class));

            // Entities with enums
            if(req.getHblTermsConditionTemplateRow() != null) {
                dest.setHblTermsConditionTemplate(req.getHblTermsConditionTemplateRow().stream()
                        .map(this::mapHblTermsCondition)
                        .toList()
                );
            }
            if(req.getHblHawbBackPrintTemplateRow() != null) {
                dest.setHblHawbBackPrintTemplate(req.getHblHawbBackPrintTemplateRow().stream()
                        .map(this::mapHblTermsCondition)
                        .toList()
                );
            }
            if(req.getTenantProducts() != null) {
                dest.setTenantProducts(req.getTenantProducts().stream()
                        .map(this::mapTenantProducts)
                        .toList()
                );
            }
            if(req.getProductSequenceConfig() != null) {
                dest.setProductSequenceConfig(req.getProductSequenceConfig().stream()
                        .map(this::mapProductSequenceConfig)
                        .toList()
                );
            }

            dest.setShipmentConsoleImportApproverRole(req.getShipmentImportApproverRole());
            dest.setLowMarginApproval(req.getIsLowMarginApprovalRequired());
            dest.setShippingInstruction(req.getShipmentInstruction());
            dest.setIsfFileMainPage(req.getISFFileMainPage());
            dest.setAirExportConsoleManifest(req.getAirExportConsolManifest());
            dest.setAirImportConsoleManifest(req.getAirImportConsolManifest());

            return shipmentSettingsService.completeSettingsUpdateCreateV1(CommonRequestModel.buildRequest(dest));
        } catch (Exception e) {
            responseMessage = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMessage, e);
            return ResponseHelper.buildFailedResponse(responseMessage);
        }
    }

    private HblTermsConditionTemplateRequest mapHblTermsCondition(HblTermsConditionTemplateDto req) {
        if(req == null)
            return null;

        var res = modelMapper.map(req, HblTermsConditionTemplateRequest.class);
        // Setting ENUMS
        try {
            res.setTypeOfHblPrint(TypeOfHblPrint.valueOf(req.getTypeOfHblPrint()));
        } catch (Exception ignored) {}

        return res;
    }

    private TenantProductsRequest mapTenantProducts(TenantProductsDto req) {
        if(req == null)
            return null;

        var res = modelMapper.map(req, TenantProductsRequest.class);
        // Setting ENUMS
        try {
            res.setProductType(ProductType.valueOf(req.getProductType()));
        } catch (Exception ignored) {}

        return res;
    }
    private ProductSequenceConfigRequest mapProductSequenceConfig(ProductSequenceConfigDto req) {
        if(req == null)
            return null;

        var res = modelMapper.map(req, ProductSequenceConfigRequest.class);
        res.setTenantProducts(mapTenantProducts(req.getTenantProductObj()));
        // Setting ENUMS
        try {
            res.setProductProcessTypes(ProductProcessTypes.valueOf(req.getProductProcessTypes()));
            res.setGenerationType(GenerationType.valueOf(req.getGenerationType()));
        } catch (Exception ignored) {}

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
        if(obj == null)
            return null;
        return modelMapper.map(obj, clazz);
    }
}
