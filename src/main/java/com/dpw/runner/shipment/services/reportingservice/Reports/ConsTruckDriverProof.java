package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.TruckDriverModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.UnitConversionUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

@Component
public class ConsTruckDriverProof extends IReport {

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
        truckDriverModel.consolidationDetails = getConsolidation(id);
        if(truckDriverModel.consolidationDetails != null && truckDriverModel.consolidationDetails.getTruckDriverDetails() != null && !truckDriverModel.consolidationDetails.getTruckDriverDetails().isEmpty())
        {
            truckDriverModel.setTruckDriverDetails(truckDriverModel.consolidationDetails.getTruckDriverDetails());
        }
        if(truckDriverModel.consolidationDetails != null) {
            List<ShipmentModel> shipments = truckDriverModel.consolidationDetails.getShipmentsList();
            if (shipments != null && !shipments.isEmpty()) {
                int sum = shipments.stream().filter(c -> c.getNoOfPacks() != null).mapToInt(ShipmentModel::getNoOfPacks).sum();
                truckDriverModel.totalPacks += sum;
                BigDecimal totalWeight = BigDecimal.ZERO;
                for (var shipment : shipments) {
                    if (shipment.getWeight() != null && shipment.getWeightUnit() != null) {
                        try {
                            var weightInKg = UnitConversionUtility.convertUnit(Constants.MASS, shipment.getWeight(), shipment.getWeightUnit(), "KG");
                            if (weightInKg != null)
                                totalWeight = totalWeight.add((BigDecimal) weightInKg);
                        } catch (Exception e) {

                        }
                    }
                }
                truckDriverModel.totalWeight = totalWeight;
            }
        }
        truckDriverModel.tenant = getTenant();
        truckDriverModel.usersDto = UserContext.getUser();
        return truckDriverModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        TruckDriverModel truckDriverModel = (TruckDriverModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(truckDriverModel.consolidationDetails, GetDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateConsolidationFields(truckDriverModel.consolidationDetails, dictionary);
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
        dictionary.put(ReportConstants.TOTAL_PACKS, truckDriverModel.totalPacks);
        dictionary.put(ReportConstants.SHIPMENT_DETAILS_TOTALWEIGHT, truckDriverModel.totalWeight);
        return dictionary;
    }
}
