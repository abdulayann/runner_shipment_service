package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TruckDriverModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
public class ShipTruckwayBillReport extends IReport {

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
        validateAirAndOceanDGCheck(truckDriverModel.shipmentDetails);
        if(truckDriverModel.shipmentDetails != null && truckDriverModel.shipmentDetails.getContainersList() != null && truckDriverModel.shipmentDetails.getContainersList().size() > 0) {
            List<ShipmentContainers> shipmentContainers = new ArrayList<>();
            for(var container: truckDriverModel.shipmentDetails.getContainersList())
            {
                shipmentContainers.add(getShipmentContainer(container));
            }
            truckDriverModel.shipmentDetails.setShipmentContainersList(shipmentContainers);
        }
        truckDriverModel.usersDto = UserContext.getUser();
        return truckDriverModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        TruckDriverModel truckDriverModel = (TruckDriverModel) documentModel;
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(truckDriverModel.shipmentDetails, GetDPWDateFormatOrDefault());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(truckDriverModel.shipmentDetails, dictionary);
        populateUserFields(truckDriverModel.usersDto, dictionary);
        return dictionary;
    }
}
