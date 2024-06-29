package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.SeawayBillModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.addCommaWithoutDecimal;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddressWithPhoneEmail;

@Component
public class SeawayBillReport extends IReport {

    public static final String GROSS_VOLUME_ALIAS = "GrossVolume";
    public static final String BL_GROSS_VOLUME_ALIAS = "BL_GrossVolume";
    public static final String BL_GROSS_WEIGHT_ALIAS = "BL_GrossWeight";
    public static final String GROSS_WEIGHT = "GrossWeight";
    public static final String NET_WEIGHT = "NetWeight";
    public static final String NOOF_PACKAGES = "NoofPackages";

    @Autowired
    private HblReport hblReport;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    @Override
    public Map<String, Object> getData(Long id) {
        SeawayBillModel seawayBillModel = (SeawayBillModel) getDocumentModel(id);
        return populateDictionary(seawayBillModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        var shipment = getShipment(id);
        var hbl = getHbl(id);
        return SeawayBillModel.builder()
                .id(id)
                .shipment(shipment)
                .blObject(hbl)
                .shipmentSettingsDetails(commonUtils.getShipmentSettingFromContext())
                .build();
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        var model = (SeawayBillModel) documentModel;
        var dict = hblReport.getData(model.getId());
        // populate missing keys from hbl doc

        dict.put(ETD_MDY, dict.get(ETD));
        dict.put(ETA_MDY, dict.get(ETA));
        dict.put(DATE_OF_RECEIPT_MDY, dict.get(DATE_OF_RECEIPT));

        dict.put(SHIPPER, dict.get(CONSIGNER));
        if(model.shipment.getConsigner() != null) {
            Map<String, Object> consignerAddress = model.shipment.getConsigner().getAddressData();
            var consignerWc = ReportHelper.getOrgAddressWithPhoneEmail(null, getValueFromMap(consignerAddress, ADDRESS1),
                    getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                    getValueFromMap(consignerAddress, EMAIL), getValueFromMap(consignerAddress, CONTACT_PHONE),
                    getValueFromMap(consignerAddress, "Zip_PostCode"));
            dict.put(SHIPPER_WC, consignerWc);
        }

        if(model.blObject != null && model.blObject.getHblData() != null){
            dict.put(CONSIGNER_ADDRESS, model.blObject.getHblData().getConsignorAddress());
            List<String> consignerWithNameAndAddress;
            List<String> consigneeWithNameAndAddress;
            if(Boolean.TRUE.equals(model.shipmentSettingsDetails.getDisableBlPartiesName())) {
                consignerWithNameAndAddress = getOrgAddressWithPhoneEmail(null, model.blObject.getHblData().getConsignorAddress(), null, null, null, null, null);
                consigneeWithNameAndAddress = getOrgAddressWithPhoneEmail(null, model.blObject.getHblData().getConsigneeAddress(), null, null, null, null, null);
            } else {
                consignerWithNameAndAddress = getOrgAddressWithPhoneEmail(model.blObject.getHblData().getConsignorName(), model.blObject.getHblData().getConsignorAddress(), null, null, null, null, null);
                consigneeWithNameAndAddress = getOrgAddressWithPhoneEmail(model.blObject.getHblData().getConsigneeName(), model.blObject.getHblData().getConsigneeAddress(), null, null, null, null, null);
            }
            dict.put("BLCustomConsigner", consignerWithNameAndAddress);
            dict.put("BLCustomConsignee", consigneeWithNameAndAddress);
            dict.put("PortOfLoad", model.blObject.getHblData().getPortOfLoad());
        }


        if (model.shipment.getShipmentContainersList() != null) {
            V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
            List<Map<String, Object>> values = jsonHelper.convertValue(model.shipment.getShipmentContainersList(), new TypeReference<>() {});
            values.forEach(v -> {
                if (v.get(GROSS_WEIGHT) != null && v.get(GROSS_WEIGHT).toString() != null)
                    v.put(GROSS_WEIGHT, ConvertToWeightNumberFormat(v.get(GROSS_WEIGHT), v1TenantSettingsResponse));
                if (v.get(NET_WEIGHT) != null && v.get(NET_WEIGHT).toString() != null)
                    v.put(NET_WEIGHT, ConvertToWeightNumberFormat(v.get(NET_WEIGHT), v1TenantSettingsResponse));
                if (v.get(NOOF_PACKAGES) != null && v.get(NOOF_PACKAGES).toString() != null)
                    v.put(NOOF_PACKAGES, GetDPWWeightVolumeFormat((BigDecimal) v.get(NOOF_PACKAGES), 0, v1TenantSettingsResponse));
                if (v.get(GROSS_VOLUME_ALIAS) != null && v.get(GROSS_VOLUME_ALIAS).toString() != null)
                    v.put(GROSS_VOLUME_ALIAS, addCommas(v.get(GROSS_VOLUME_ALIAS).toString()));
                if (v.get(BL_GROSS_VOLUME_ALIAS) != null && v.get(BL_GROSS_VOLUME_ALIAS).toString() != null)
                    v.put(BL_GROSS_VOLUME_ALIAS, addCommas(v.get(BL_GROSS_VOLUME_ALIAS).toString()));
                if (v.get(BL_GROSS_WEIGHT_ALIAS) != null && v.get(BL_GROSS_WEIGHT_ALIAS).toString() != null)
                    v.put(BL_GROSS_WEIGHT_ALIAS, ConvertToWeightNumberFormat(v.get(BL_GROSS_WEIGHT_ALIAS), v1TenantSettingsResponse));
            });
            dict.put("ShipmentContainers", values);
        }


        return dict;
    }
}
