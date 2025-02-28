package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.stereotype.Service;

import java.util.Map;

@Service
public class ShipmentTagsForExteranlServices extends IReport{

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        return null;
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        return null;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return null;
    }
}
