package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.MawbModel;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class MawbReport extends IReport{

    @Override
    public Map<String, Object> getData(Long id) {
        MawbModel mawbModel = (MawbModel) getDocumentModel(id);
        return populateDictionary(mawbModel);
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
