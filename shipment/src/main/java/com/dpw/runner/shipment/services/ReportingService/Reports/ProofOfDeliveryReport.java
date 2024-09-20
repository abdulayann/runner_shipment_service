package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import java.util.Collections;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class ProofOfDeliveryReport extends IReport {

    @Autowired
    private HblReport hblReport;

    @Override
    public Map<String, Object> getData(Long id) {
        return hblReport.getData(id);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        return null;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return Collections.emptyMap();
    }
}
