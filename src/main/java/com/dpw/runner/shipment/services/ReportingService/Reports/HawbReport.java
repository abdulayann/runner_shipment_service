package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class HawbReport extends IReport{

    @Override
    public Map<String, Object> getData(Long id) {
        HawbModel hawbModel = (HawbModel) getDocumentModel(id);
        return populateDictionary(hawbModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        return null;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return null;
    }
}
