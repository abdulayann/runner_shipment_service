package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.SeawayBillModel;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.addCommaWithoutDecimal;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddressWithPhoneEmail;

@Component
public class SeawayBillReport extends IReport {

    @Autowired
    HblReport hblReport;
    @Autowired
    IShipmentDao shipmentDao;
    @Autowired
    JsonHelper jsonHelper;

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
            var consignerWithNameAndAddress = getOrgAddressWithPhoneEmail(model.blObject.getHblData().getConsignorName(), model.blObject.getHblData().getConsignorAddress(), null, null, null, null, null);
            var consigneeWithNameAndAddress = getOrgAddressWithPhoneEmail(model.blObject.getHblData().getConsigneeName(), model.blObject.getHblData().getConsigneeAddress(), null, null, null, null, null);
            dict.put("BLCustomConsigner", consignerWithNameAndAddress);
            dict.put("BLCustomConsignee", consigneeWithNameAndAddress);
            dict.put("PortOfLoad", model.blObject.getHblData().getPortOfLoad());
        }


        if (model.shipment.getShipmentContainersList() != null) {
            V1TenantSettingsResponse v1TenantSettingsResponse = getTenantSettings();
//            String json = jsonHelper.convertToJson(model.shipment.getShipmentContainersList());
            var values = model.shipment.getShipmentContainersList().stream()
                    .map(i -> jsonHelper.convertJsonToMap(jsonHelper.convertToJson(i)))
                    .toList();
//            var values = jsonHelper.convertValue(json, new TypeReference<List<Map<String, Object>>>() {});
            values.forEach(v -> {
                if (v.get("GrossWeight") != null && v.get("GrossWeight").toString() != null)
                    v.put("GrossWeight", ConvertToWeightNumberFormat(v.get("GrossWeight"), v1TenantSettingsResponse));
                if (v.get("NetWeight") != null && v.get("NetWeight").toString() != null)
                    v.put("NetWeight", ConvertToWeightNumberFormat(v.get("NetWeight"), v1TenantSettingsResponse));
                if (v.get("NoofPackages") != null && v.get("NoofPackages").toString() != null)
                    v.put("NoofPackages", addCommaWithoutDecimal((BigDecimal) v.get("NoofPackages")));
                if (v.get("GrossVolume") != null && v.get("GrossVolume").toString() != null)
                    v.put("GrossVolume", addCommas(v.get("GrossVolume").toString()));
                if (v.get("BL_GrossVolume") != null && v.get("BL_GrossVolume").toString() != null)
                    v.put("BL_GrossVolume", addCommas(v.get("BL_GrossVolume").toString()));
                if (v.get("BL_GrossWeight") != null && v.get("BL_GrossWeight").toString() != null)
                    v.put("BL_GrossWeight", ConvertToWeightNumberFormat(v.get("BL_GrossWeight"), v1TenantSettingsResponse));
            });
            dict.put("ShipmentContainers", values);
        }


        return dict;
    }
}
