package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.FCRDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddressDetails;

@Component
public class FCRDocumentReport extends IReport{

    @Autowired
    private ModelMapper modelMapper;

    public void setFcrShipper(PartiesRequest fcrShipper1) {
        this.fcrShipper = fcrShipper1;
    }

    private PartiesRequest fcrShipper;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        FCRDocumentModel fcrDocumentModel = (FCRDocumentModel) getDocumentModel(id);
        return populateDictionary(fcrDocumentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        FCRDocumentModel fcrDocumentModel = new FCRDocumentModel();
        fcrDocumentModel.setShipmentModel(getShipment(id));
        return fcrDocumentModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) throws RunnerException {
        FCRDocumentModel fcrDocumentModel = (FCRDocumentModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        if(fcrShipper != null)
            dictionary.put(ReportConstants.FCR_CONSIGNOR_IN_CAPS, getOrgAddressDetails(modelMapper.map(fcrDocumentModel, PartiesModel.class)));
        dictionary.put(SHIP_CONSIGNEE_IN_CAPS, getOrgAddressDetails(fcrDocumentModel.getShipmentModel().getConsignee()));
        dictionary.put(SHIP_NOTIFY_PARTY_IN_CAPS, getOrgAddressDetails(fcrDocumentModel.getShipmentModel().getAdditionalDetails().getNotifyParty()));
        dictionary.put(SHIPMENT_NO, fcrDocumentModel.getShipmentModel().getShipmentId());
        dictionary.put(FCR_NO, fcrDocumentModel.getShipmentModel().getShipmentId() + fcrDocumentModel.getShipmentModel().getAdditionalDetails().getFcrNumber() + 1);
        getPackingDetails(fcrDocumentModel.getShipmentModel(), dictionary);
        dictionary.put(PLACE_OF_ISSUE, fcrDocumentModel.getShipmentModel().getAdditionalDetails().getPlaceOfIssue());
        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE, fcrDocumentModel.getShipmentModel().getAdditionalDetails().getDateOfIssue());
        convertValuesToUpperCase(dictionary);
        return dictionary;
    }

    public static Map<String, Object> convertValuesToUpperCase(Map<String, Object> map) {
        Map<String, Object> result = new HashMap<>();
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (value instanceof String) {
                result.put(key, StringUtility.toUpperCase((String) value));
            } else if (value instanceof List) {
                // Convert a list of strings to uppercase
                List<?> list = (List<?>) value;
                result.put(key, convertListToUpperCase(list));
            } else if (value instanceof Map) {
                // Recursively handle nested maps
                Map<?, ?> nestedMap = (Map<?, ?>) value;
                result.put(key, convertValuesToUpperCase((Map<String, Object>) nestedMap));
            } else {
                // Keep other types as is
                result.put(key, value);
            }
        }
        return result;
    }

    private static List<?> convertListToUpperCase(List<?> list) {
        List<Object> result = new ArrayList<>();
        for (Object item : list) {
            if (item instanceof String) {
                result.add(StringUtility.toUpperCase((String) item));
            } else if (item instanceof Map) {
                // Recursively handle nested maps in lists
                result.add(convertValuesToUpperCase((Map<String, Object>) item));
            } else {
                // Keep other types as is
                result.add(item);
            }
        }
        return result;
    }
}
