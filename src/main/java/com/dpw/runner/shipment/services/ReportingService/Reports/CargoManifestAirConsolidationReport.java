package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class CargoManifestAirConsolidationReport extends IReport{
    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        return null;
    }

    public Map<String, Object> getData(Long shipmentId, Long consolidationId) throws RunnerException {
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
