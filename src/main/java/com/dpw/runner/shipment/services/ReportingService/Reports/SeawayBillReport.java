package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNER_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PHONE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DATE_OF_RECEIPT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DATE_OF_RECEIPT_MDY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EMAIL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA_MDY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD_MDY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPER_WC;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddressWithPhoneEmail;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.SeawayBillModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@NoArgsConstructor
public class SeawayBillReport extends IReport {

    public static final String GROSS_VOLUME_ALIAS = "GrossVolume";
    public static final String BL_GROSS_VOLUME_ALIAS = "BL_GrossVolume";
    public static final String BL_GROSS_WEIGHT_ALIAS = "BL_GrossWeight";
    public static final String GROSS_WEIGHT = "GrossWeight";
    public static final String NET_WEIGHT = "NetWeight";
    public static final String NOOF_PACKAGES = "NoofPackages";

    private HblReport hblReport;
    private JsonHelper jsonHelper;
    private CommonUtils commonUtils;
    private ShipmentService shipmentService;

    private V1TenantSettingsResponse tenantSettings;

    @Autowired
    public SeawayBillReport(HblReport hblReport, JsonHelper jsonHelper, CommonUtils commonUtils, ShipmentService shipmentService) {
        this.hblReport = hblReport;
        this.jsonHelper = jsonHelper;
        this.commonUtils = commonUtils;
        this.shipmentService = shipmentService;
    }

    @Override
    public Map<String, Object> getData(Long id) {
        validatePrinting(id);
        SeawayBillModel seawayBillModel = (SeawayBillModel) getDocumentModel(id);
        return populateDictionary(seawayBillModel);
    }

    public void validatePrinting(Long shipmentId) {
        tenantSettings = getCurrentTenantSettings();
        ShipmentSettingsDetails shipmentSettingFromContext = commonUtils.getShipmentSettingFromContext();
        ShipmentDetails shipment = getShipmentDetailsOrThrow(shipmentId);

        if (Boolean.TRUE.equals(tenantSettings.getIsModuleValidationEnabled())) {
            validateShipmentModules(shipment);
        }

        validateUnassignedPackagesForSeaway(shipment, shipmentSettingFromContext);
    }

    private ShipmentDetails getShipmentDetailsOrThrow(Long shipmentId) {
        ShipmentDetails shipment = getShipmentDetails(shipmentId);
        if (shipment == null) {
            throw new ReportException("No shipment found with id: " + shipmentId);
        }
        return shipment;
    }

    private void validateShipmentModules(ShipmentDetails shipment) {
        if (shouldValidateModules(shipment)) {
            List<ModuleValidationFieldType> missingFields = new ArrayList<>();
            shipmentService.validateCarrierDetails(shipment, missingFields);
            shipmentService.validateContainerDetails(shipment, missingFields);

            if (ObjectUtils.isNotEmpty(missingFields)) {
                String errorMessage = missingFields.stream()
                        .map(ModuleValidationFieldType::getDescription)
                        .collect(Collectors.joining(" | "));
                throw new ReportException(errorMessage);
            }
        }
    }

    private boolean shouldValidateModules(ShipmentDetails shipment) {
        return Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())
                && Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                && (Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                || Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipment.getShipmentType()))
                && ObjectUtils.isNotEmpty(shipment.getJobType())
                && !Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType());
    }

    private void validateUnassignedPackagesForSeaway(ShipmentDetails shipment, ShipmentSettingsDetails shipmentSettingFromContext) {
        validateUnassignedPackagesInternal(
                shipment,
                shipmentSettingFromContext,
                "Seaway Bill",
                "Seaway for possible cargo discrepancies."
        );
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
            processConsigner(model, dict);
        }

        if(model.blObject != null && model.blObject.getHblData() != null){
            processHblData(model, dict);
        }

        if (model.shipment.getShipmentContainersList() != null) {
            processShipmentContainerList(model, dict);
        }

        if (model.getConsolidation() != null) {
            this.populateConsolidationReportData(dict, null, model.getConsolidation().getId());
        }

        if (model.getShipment() != null) {
            this.populateShipmentReportData(dict, null, model.getShipment().getId());
            this.getContainerDetails(model.getShipment(), dict);
            this.getPackingDetails(model.getShipment(), dict);
        }

        return dict;
    }

    private void processShipmentContainerList(SeawayBillModel model, Map<String, Object> dict) {
        tenantSettings = getCurrentTenantSettings();
        List<Map<String, Object>> values = jsonHelper.convertValue(model.shipment.getShipmentContainersList(), new TypeReference<>() {
        });
        values.forEach(v -> {
            processGrossWeight(v);
            processNetWeight(v);
            processNoofPackages(v);
            processGrossVolumeAlias(v);
            processBlGrossVolumeAlias(v);
            processBlGrossWeightAlias(v);
        });
        dict.put("ShipmentContainers", values);
    }

    private void processBlGrossWeightAlias(Map<String, Object> v) {
        if (v.get(BL_GROSS_WEIGHT_ALIAS) != null && v.get(BL_GROSS_WEIGHT_ALIAS).toString() != null) {
            v.put(BL_GROSS_WEIGHT_ALIAS, convertToWeightNumberFormat(v.get(BL_GROSS_WEIGHT_ALIAS), tenantSettings));
        }
    }

    private void processBlGrossVolumeAlias(Map<String, Object> v) {
        if (v.get(BL_GROSS_VOLUME_ALIAS) != null && v.get(BL_GROSS_VOLUME_ALIAS).toString() != null) {
            v.put(BL_GROSS_VOLUME_ALIAS, addCommas(v.get(BL_GROSS_VOLUME_ALIAS).toString()));
        }
    }

    private void processGrossVolumeAlias(Map<String, Object> v) {
        if (v.get(GROSS_VOLUME_ALIAS) != null && v.get(GROSS_VOLUME_ALIAS).toString() != null) {
            v.put(GROSS_VOLUME_ALIAS, addCommas(v.get(GROSS_VOLUME_ALIAS).toString()));
        }
    }

    private void processNoofPackages(Map<String, Object> v) {
        if (v.get(NOOF_PACKAGES) != null && v.get(NOOF_PACKAGES).toString() != null) {
            v.put(NOOF_PACKAGES, getDPWWeightVolumeFormat((BigDecimal) v.get(NOOF_PACKAGES), 0, tenantSettings));
        }
    }

    private void processNetWeight(Map<String, Object> v) {
        if (v.get(NET_WEIGHT) != null && v.get(NET_WEIGHT).toString() != null) {
            v.put(NET_WEIGHT, convertToWeightNumberFormat(v.get(NET_WEIGHT), tenantSettings));
        }
    }

    private void processGrossWeight(Map<String, Object> v) {
        if (v.get(GROSS_WEIGHT) != null && v.get(GROSS_WEIGHT).toString() != null) {
            v.put(GROSS_WEIGHT, convertToWeightNumberFormat(v.get(GROSS_WEIGHT), tenantSettings));
        }
    }

    private void processHblData(SeawayBillModel model, Map<String, Object> dict) {
        dict.put(CONSIGNER_ADDRESS, model.blObject.getHblData().getConsignorAddress());
        List<String> consignerWithNameAndAddress;
        List<String> consigneeWithNameAndAddress;
        if (Boolean.TRUE.equals(model.shipmentSettingsDetails.getDisableBlPartiesName())) {
            consignerWithNameAndAddress = getOrgAddressWithPhoneEmail(null, model.blObject.getHblData().getConsignorAddress(), null, null, null, null, null);
            consigneeWithNameAndAddress = getOrgAddressWithPhoneEmail(null, model.blObject.getHblData().getConsigneeAddress(), null, null, null, null, null);
        } else {
            consignerWithNameAndAddress = getOrgAddressWithPhoneEmail(model.blObject.getHblData().getConsignorName(), model.blObject.getHblData().getConsignorAddress(), null, null,
                    null, null, null);
            consigneeWithNameAndAddress = getOrgAddressWithPhoneEmail(model.blObject.getHblData().getConsigneeName(), model.blObject.getHblData().getConsigneeAddress(), null, null,
                    null, null, null);
        }
        dict.put("BLCustomConsigner", consignerWithNameAndAddress);
        dict.put("BLCustomConsignee", consigneeWithNameAndAddress);
        dict.put("PortOfLoad", model.blObject.getHblData().getPortOfLoad());
    }

    private void processConsigner(SeawayBillModel model, Map<String, Object> dict) {
        Map<String, Object> consignerAddress = model.shipment.getConsigner().getAddressData();
        var consignerWc = ReportHelper.getOrgAddressWithPhoneEmail(null, getValueFromMap(consignerAddress, ADDRESS1),
                getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                getValueFromMap(consignerAddress, EMAIL), getValueFromMap(consignerAddress, CONTACT_PHONE),
                getValueFromMap(consignerAddress, "Zip_PostCode"));
        dict.put(SHIPPER_WC, consignerWc);
    }
}
