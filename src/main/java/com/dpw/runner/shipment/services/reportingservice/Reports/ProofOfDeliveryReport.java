package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.Models.HblModel;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ProofOfDeliveryModel;
import java.util.ArrayList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class ProofOfDeliveryReport extends IReport {

    @Autowired
    private HblReport hblReport;

    @Override
    public Map<String, Object> getData(Long id) {
        ProofOfDeliveryModel proofOfDeliveryModel = (ProofOfDeliveryModel) getDocumentModel(id);
        return populateDictionary(proofOfDeliveryModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        ProofOfDeliveryModel proofOfDeliveryModel = new ProofOfDeliveryModel();
        proofOfDeliveryModel.hblModel= (HblModel) hblReport.getDocumentModel(id);
        return proofOfDeliveryModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ProofOfDeliveryModel proofOfDeliveryModel = (ProofOfDeliveryModel) documentModel;
        Map<String, Object> dictionary = hblReport.populateDictionary(proofOfDeliveryModel.hblModel);
        if(proofOfDeliveryModel.hblModel.shipment != null) {
            populateShipmentOrganizationsLL(proofOfDeliveryModel.hblModel.shipment, dictionary,
                new ArrayList<>());
        }
        return dictionary;
    }
}
