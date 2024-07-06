package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.CustomsInstructionsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

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
        validateAirDGCheck(customsInstructionsModel.shipmentDetails);
        if(customsInstructionsModel.shipmentDetails.getContainersList() != null && customsInstructionsModel.shipmentDetails.getContainersList().size() > 0) {
            List<ShipmentContainers> shipmentContainersList = new ArrayList<>();
            for (ContainerModel containerModel: customsInstructionsModel.shipmentDetails.getContainersList()) {
                ShipmentContainers shipmentContainers = getShipmentContainer(containerModel);
                shipmentContainersList.add(shipmentContainers);
            }
            if(shipmentContainersList.size() > 0)
                customsInstructionsModel.setShipmentContainers(shipmentContainersList);
        }
        customsInstructionsModel.shipmentDetails.setShipmentContainersList(customsInstructionsModel.getShipmentContainers());
        return customsInstructionsModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CustomsInstructionsModel customsInstructionsModel = (CustomsInstructionsModel) documentModel;
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(customsInstructionsModel.shipmentDetails, GetDPWDateFormatOrDefault());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(((CustomsInstructionsModel) documentModel).shipmentDetails, dictionary);
        JsonDateFormat(dictionary);
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
        // TODO- Logo Path
        if (customsInstructionsModel.shipmentDetails.getPackingList() != null && customsInstructionsModel.shipmentDetails.getPackingList().size() > 0) {
            dictionary.put(ReportConstants.HAS_PACKAGES, true);
            dictionary.put(ReportConstants.PACKING_LIST, getPackingDetails(customsInstructionsModel.shipmentDetails, dictionary));
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        if (customsInstructionsModel.shipmentDetails.getCarrierDetails() != null)
            dictionary.put(ReportConstants.ETD, ConvertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        if (customsInstructionsModel.shipmentDetails.getCarrierDetails() != null)
            dictionary.put(ReportConstants.ETA, ConvertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if (customsInstructionsModel.shipmentDetails.getAdditionalDetails() != null)
            dictionary.put(ReportConstants.DATE_OF_ISSUE, ConvertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getAdditionalDetails().getDateOfIssue(), tsDateTimeFormat));
        if (customsInstructionsModel.getShipmentContainers() != null) {
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, customsInstructionsModel.getShipmentContainers());
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ShipmentContainers shipmentContainers : customsInstructionsModel.getShipmentContainers()) {
                valuesContainer.add(jsonHelper.convertValue(shipmentContainers, new TypeReference<>() {}));
            }
            for (Map<String, Object> v : valuesContainer) {
                if(v.containsKey(ReportConstants.GROSS_VOLUME) && v.get(ReportConstants.GROSS_VOLUME) != null)
                    v.put(ReportConstants.GROSS_VOLUME, ConvertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.GROSS_WEIGHT) && v.get(ReportConstants.GROSS_WEIGHT) != null)
                    v.put(ReportConstants.GROSS_WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.NET_WEIGHT) && v.get(ReportConstants.NET_WEIGHT) != null)
                    v.put(ReportConstants.NET_WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.NET_WEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.SHIPMENT_PACKS) && v.get(ReportConstants.SHIPMENT_PACKS) != null)
                    v.put(ReportConstants.SHIPMENT_PACKS, addCommaWithoutDecimal(new BigDecimal(v.get(ReportConstants.SHIPMENT_PACKS).toString())));
                if (v.containsKey(ReportConstants.TareWeight) && v.get(ReportConstants.TareWeight) != null)
                    v.put(ReportConstants.TareWeight, ConvertToWeightNumberFormat(v.get(ReportConstants.TareWeight), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.VGMWeight) && v.get(ReportConstants.VGMWeight) != null)
                    v.put(ReportConstants.VGMWeight, ConvertToWeightNumberFormat(v.get(ReportConstants.VGMWeight), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.MIN_TEMP) && v.get(ReportConstants.MIN_TEMP) != null)
                    v.put(ReportConstants.MIN_TEMP, ConvertToWeightNumberFormat(v.get(ReportConstants.MIN_TEMP), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.MAX_TEMP) && v.get(ReportConstants.MAX_TEMP) != null)
                    v.put(ReportConstants.MAX_TEMP, ConvertToWeightNumberFormat(v.get(ReportConstants.MAX_TEMP), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.NO_OF_PACKAGES) && v.get(ReportConstants.NO_OF_PACKAGES) != null)
                    v.put(ReportConstants.NO_OF_PACKAGES, GetDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(v.get(ReportConstants.NO_OF_PACKAGES))), 0, v1TenantSettingsResponse));
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        }
        return dictionary;
    }
}
