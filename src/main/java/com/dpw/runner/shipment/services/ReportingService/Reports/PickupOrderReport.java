package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class PickupOrderReport extends IReport {

    @Override
    public Map<String, Object> getData(Long id) {
        HblReport hblReport = new HblReport();
        return hblReport.getData(id);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        return null;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return null;
    }
}
