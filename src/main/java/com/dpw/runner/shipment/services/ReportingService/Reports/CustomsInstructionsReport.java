package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CustomsInstructionsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
        return customsInstructionsModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CustomsInstructionsModel customsInstructionsModel = (CustomsInstructionsModel) documentModel;
        String json = jsonHelper.convertToJson(customsInstructionsModel.shipmentDetails);
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        // TODO- what is ShipmentContainers
        JsonDateFormat(dictionary);
        List<String> consigner = getOrgAddress(customsInstructionsModel.shipmentDetails.getConsigner());
        List<String> consignee = getOrgAddress(customsInstructionsModel.shipmentDetails.getConsignee());
        List<String> exportBroker = getOrgAddress(customsInstructionsModel.shipmentDetails.getAdditionalDetails().getExportBroker());
        List<String> importBroker = getOrgAddress(customsInstructionsModel.shipmentDetails.getAdditionalDetails().getImportBroker());
        dictionary.put(ReportConstants.CONSIGNER, consigner);
        dictionary.put(ReportConstants.CONSIGNEE, consignee);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.CONSIGNER_ADDRESS, getAddressList(ReportHelper.getValueFromMap(customsInstructionsModel.shipmentDetails.getConsigner().getAddressData(), ReportConstants.ADDRESS1)));
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, getAddressList(ReportHelper.getValueFromMap(customsInstructionsModel.shipmentDetails.getConsignee().getAddressData(), ReportConstants.ADDRESS1)));
        dictionary.put(ReportConstants.EXPORT_BROKER, exportBroker);
        dictionary.put(ReportConstants.IMPORT_BROKER, importBroker);
        dictionary.put(ReportConstants.VESSEL_NAME, customsInstructionsModel.shipmentDetails.getCarrierDetails().getVessel());
        // TODO- Logo Path
        if(customsInstructionsModel.shipmentDetails.getPackingList() != null && customsInstructionsModel.shipmentDetails.getPackingList().size() > 0) {
            dictionary.put(ReportConstants.HAS_PACKAGES, true);
        }
        if(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEtd() != null)
            dictionary.put(ReportConstants.ETD, ConvertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEtd()));
        if(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEta() != null)
            dictionary.put(ReportConstants.ETA, ConvertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getCarrierDetails().getEta()));
        if(customsInstructionsModel.shipmentDetails.getAdditionalDetails().getDateOfIssue() != null)
            dictionary.put(ReportConstants.DATE_OF_ISSUE, ConvertToDPWDateFormat(customsInstructionsModel.shipmentDetails.getAdditionalDetails().getDateOfIssue()));
        return dictionary;
    }
}
