package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class MawbReport extends IReport{

    @Autowired
    private HawbReport hawbReport;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private ModelMapper modelMapper;

    public boolean isDMawb;


    @Override
    public Map<String, Object> getData(Long id) {
        HawbModel mawbModel = (HawbModel) getDocumentModel(id);
        return populateDictionary(mawbModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HawbModel hawbModel = new HawbModel();
        if(!isDMawb) {
            hawbModel.usersDto = UserContext.getUser();
            hawbModel.setConsolidationDetails(getConsolidation(id));
            validateAirDGCheckConsolidations(hawbModel.getConsolidationDetails());
            String entityType = "MAWB";
            hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
            hawbModel.awb = hawbModel.getMawb();
            hawbModel.setEntityType(entityType);
        } else {
            hawbModel.usersDto = UserContext.getUser();
            hawbModel.shipmentDetails = getShipment(id);
            validateAirDGCheckShipments(hawbModel.shipmentDetails);
            String entityType = "MAWB";
            if(hawbModel.shipmentDetails != null && hawbModel.shipmentDetails.getConsolidationList() != null && !hawbModel.shipmentDetails.getConsolidationList().isEmpty())
            {
                hawbModel.setConsolidationDetails(hawbModel.shipmentDetails.getConsolidationList().get(0));
                hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
                hawbModel.awb = hawbModel.getMawb();
            }
            if(hawbModel.getMawb() == null){
                hawbModel.awb = getHawb(id);
                entityType = "DMAWB";
            }
            hawbModel.setEntityType(entityType);
        }

        return hawbModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        HawbModel model = (HawbModel) documentModel;
        var dictionary =  hawbReport.populateDictionary(documentModel);
        if(model.getConsolidationDetails() != null) {
            populateRaKcDataConsolidation(dictionary, model.getConsolidationDetails());
        }
        return dictionary;
    }
}
