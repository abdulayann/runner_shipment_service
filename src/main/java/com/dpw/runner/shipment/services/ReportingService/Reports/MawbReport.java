package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.MawbModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class MawbReport extends IReport{

    @Autowired
    private HawbReport hawbReport;

    @Override
    public Map<String, Object> getData(Long id) {
        HawbModel mawbModel = (HawbModel) getDocumentModel(id);
        return populateDictionary(mawbModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HawbModel hawbModel = new HawbModel();
        hawbModel.usersDto = UserContext.getUser();
        hawbModel.shipmentDetails = getShipment(id);
        if(hawbModel.shipmentDetails != null && hawbModel.shipmentDetails.getConsolidationList() != null && !hawbModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            hawbModel.setConsolidationDetails(hawbModel.shipmentDetails.getConsolidationList().get(0));
            hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
        }
        hawbModel.awb = hawbModel.getMawb();
        hawbModel.setEntityType("HAWB");
        return hawbModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return hawbReport.populateDictionary(documentModel);
    }
}
