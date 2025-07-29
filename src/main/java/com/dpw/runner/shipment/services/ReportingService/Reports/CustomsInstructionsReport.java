package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.addCommaWithoutDecimal;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getAddressList;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.CustomsInstructionsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CustomsInstructionsReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        CustomsInstructionsModel customsInstructionsModel = (CustomsInstructionsModel) getDocumentModel(id);
        return populateDictionary(customsInstructionsModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        CustomsInstructionsModel customsInstructionsModel = new CustomsInstructionsModel();
        customsInstructionsModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(customsInstructionsModel.shipmentDetails);
        if(customsInstructionsModel.shipmentDetails.getContainersList() != null && !customsInstructionsModel.shipmentDetails.getContainersList().isEmpty()) {
            List<ShipmentContainers> shipmentContainersList = new ArrayList<>();
            for (ContainerModel containerModel: customsInstructionsModel.shipmentDetails.getContainersList()) {
                ShipmentContainers shipmentContainers = getShipmentContainer(containerModel);
                shipmentContainersList.add(shipmentContainers);
            }
            if(!shipmentContainersList.isEmpty())
                customsInstructionsModel.setShipmentContainers(shipmentContainersList);
        }
        customsInstructionsModel.shipmentDetails.setShipmentContainersList(customsInstructionsModel.getShipmentContainers());
        return customsInstructionsModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CustomsInstructionsModel customsInstructionsModel = (CustomsInstructionsModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(customsInstructionsModel.shipmentDetails, getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(((CustomsInstructionsModel) documentModel).shipmentDetails, dictionary);
        jsonDateFormat(dictionary);
        List<String> consigner = getOrgAddress(customsInstructionsModel.shipmentDetails.getConsigner());
        List<String> consignee = getOrgAddress(customsInstructionsModel.shipmentDetails.getConsignee());
        List<String> exportBroker = getOrgAddress(customsInstructionsModel.shipmentDetails.getAdditionalDetails() != null ?
                customsInstructionsModel.shipmentDetails.getAdditionalDetails().getExportBroker() : null);
        List<String> importBroker = getOrgAddress(customsInstructionsModel.shipmentDetails.getAdditionalDetails() != null ?
                customsInstructionsModel.shipmentDetails.getAdditionalDetails().getImportBroker() : null);
        dictionary.put(ReportConstants.CONSIGNER, consigner);
        dictionary.put(ReportConstants.CONSIGNEE, consignee);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.CONSIGNER_ADDRESS, getAddressList(ReportHelper.getValueFromMap(customsInstructionsModel.shipmentDetails.getConsigner() != null ?
                customsInstructionsModel.shipmentDetails.getConsigner().getAddressData() : null, ReportConstants.ADDRESS1)));
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, getAddressList(ReportHelper.getValueFromMap(customsInstructionsModel.shipmentDetails.getConsignee() != null ?
                customsInstructionsModel.shipmentDetails.getConsignee().getAddressData() : null, ReportConstants.ADDRESS1)));
        dictionary.put(ReportConstants.EXPORT_BROKER, exportBroker);
        dictionary.put(ReportConstants.IMPORT_BROKER, importBroker);
        if(customsInstructionsModel.shipmentDetails.getCarrierDetails() != null) {
            VesselsResponse vesselsResponse = getVesselsData(customsInstructionsModel.shipmentDetails.getCarrierDetails().getVessel());
            if(vesselsResponse != null)
                dictionary.put(ReportConstants.VESSEL_NAME, vesselsResponse.getName());
        }
        // LATER- Logo Path
        if (customsInstructionsModel.shipmentDetails.getPackingList() != null && !customsInstructionsModel.shipmentDetails.getPackingList().isEmpty()) {
            dictionary.put(ReportConstants.HAS_PACKAGES, true);
            dictionary.put(ReportConstants.PACKING_LIST, getPackingDetails(customsInstructionsModel.shipmentDetails, dictionary));
        }
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        if (customsInstructionsModel.shipmentDetails.getCarrierDetails() != null)
            dictionary.put(ReportConstants.ETD, convertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
        if (customsInstructionsModel.shipmentDetails.getCarrierDetails() != null)
            dictionary.put(ReportConstants.ETA, convertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
        if (customsInstructionsModel.shipmentDetails.getAdditionalDetails() != null)
            dictionary.put(ReportConstants.DATE_OF_ISSUE, convertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getAdditionalDetails().getDateOfIssue(), tsDateTimeFormat, v1TenantSettingsResponse));
        processShipmentContainers(customsInstructionsModel, dictionary, v1TenantSettingsResponse);

        if (customsInstructionsModel.shipmentDetails != null && ObjectUtils.isNotEmpty(customsInstructionsModel.shipmentDetails.getConsolidationList())) {
            ConsolidationModel consolidationModel = customsInstructionsModel.shipmentDetails.getConsolidationList().get(0);
            this.populateConsolidationReportData(dictionary, null, consolidationModel.getId());
        }

        if(customsInstructionsModel.shipmentDetails != null) {
            this.populateShipmentReportData(dictionary, null, customsInstructionsModel.shipmentDetails.getId());
            this.getPackingDetails(customsInstructionsModel.shipmentDetails, dictionary);
            this.getContainerDetails(customsInstructionsModel.shipmentDetails, dictionary);
        }
        return dictionary;
    }

    private void processShipmentContainers(CustomsInstructionsModel customsInstructionsModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (customsInstructionsModel.getShipmentContainers() != null) {
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, customsInstructionsModel.getShipmentContainers());
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ShipmentContainers shipmentContainers : customsInstructionsModel.getShipmentContainers()) {
                valuesContainer.add(jsonHelper.convertValue(shipmentContainers, new TypeReference<>() {}));
            }
            processValuesContainer(v1TenantSettingsResponse, valuesContainer);
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        }
    }

    private void processValuesContainer(V1TenantSettingsResponse v1TenantSettingsResponse, List<Map<String, Object>> valuesContainer) {
        for (Map<String, Object> v : valuesContainer) {
            updateValue(v, ReportConstants.GROSS_VOLUME, convertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.GROSS_WEIGHT, convertToVolumeNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.NET_WEIGHT, convertToVolumeNumberFormat(v.get(ReportConstants.NET_WEIGHT), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.TARE_WEIGHT, convertToVolumeNumberFormat(v.get(ReportConstants.TARE_WEIGHT), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.VGM_WEIGHT, convertToVolumeNumberFormat(v.get(ReportConstants.VGM_WEIGHT), v1TenantSettingsResponse));
            if (v.containsKey(ReportConstants.SHIPMENT_PACKS) && v.get(ReportConstants.SHIPMENT_PACKS) != null)
                updateValue(v, ReportConstants.SHIPMENT_PACKS, addCommaWithoutDecimal(new BigDecimal(v.get(ReportConstants.SHIPMENT_PACKS).toString())));
            updateValue(v, ReportConstants.MIN_TEMP, convertToWeightNumberFormat(v.get(ReportConstants.MIN_TEMP), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.MAX_TEMP, convertToWeightNumberFormat(v.get(ReportConstants.MAX_TEMP), v1TenantSettingsResponse));
            updateValue(v, ReportConstants.NO_OF_PACKAGES, getDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(v.get(ReportConstants.NO_OF_PACKAGES))), 0, v1TenantSettingsResponse));
        }
    }

    private void updateValue(Map<String, Object> map, String key, Object newValue) {
        if (map.containsKey(key) && map.get(key) != null) {
            map.put(key, newValue);
        }
    }

}
