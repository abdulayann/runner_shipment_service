package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collections;
import java.util.Map;

public class DeliveryOrder extends IReport {

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
