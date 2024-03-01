package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TruckDriverModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
public class ShipTruckDriverProof extends IReport {

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        TruckDriverModel truckDriverModel = (TruckDriverModel) getDocumentModel(id);
        return populateDictionary(truckDriverModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        TruckDriverModel truckDriverModel = new TruckDriverModel();
        truckDriverModel.shipmentDetails = getShipment(id);
        if(truckDriverModel.shipmentDetails != null && truckDriverModel.shipmentDetails.getContainersList() != null && truckDriverModel.shipmentDetails.getContainersList().size() > 0) {
            List<ShipmentContainers> shipmentContainers = new ArrayList<>();
            for(var container: truckDriverModel.shipmentDetails.getContainersList())
            {
                shipmentContainers.add(getShipmentContainer(container));
            }
            truckDriverModel.shipmentDetails.setShipmentContainersList(shipmentContainers);
        }
        if(truckDriverModel.shipmentDetails != null && truckDriverModel.shipmentDetails.getTruckDriverDetails() != null && truckDriverModel.shipmentDetails.getTruckDriverDetails().size() > 0)
        {
            truckDriverModel.setTruckDriverDetails(truckDriverModel.shipmentDetails.getTruckDriverDetails());
        }
        truckDriverModel.tenant = getTenant();
        truckDriverModel.usersDto = UserContext.getUser();
        return truckDriverModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        TruckDriverModel truckDriverModel = (TruckDriverModel) documentModel;
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(truckDriverModel.shipmentDetails, GetDPWDateFormatOrDefault());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(truckDriverModel.shipmentDetails, false, dictionary);
        populateUserFields(truckDriverModel.usersDto, dictionary);
        populateTenantFields(dictionary, truckDriverModel.tenant);
        for(var truckDriver: truckDriverModel.getTruckDriverDetails())
        {
            if(StringUtility.isEmpty(truckDriver.getSelfTransporterName()))
            {
                truckDriver.setTransporterName(""); //TODO - fetch transporter real name
            }
            else
                truckDriver.setTransporterName(truckDriver.getSelfTransporterName());
        }
        dictionary.put(ReportConstants.SHIPMENT_TRUCKDRIVERDETAILS, truckDriverModel.getTruckDriverDetails());
        return dictionary;
    }
}
