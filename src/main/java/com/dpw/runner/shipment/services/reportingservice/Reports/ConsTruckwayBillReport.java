package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.TruckDriverModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
public class ConsTruckwayBillReport extends IReport {

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
        if(truckDriverModel.consolidationDetails != null && truckDriverModel.consolidationDetails.getContainersList() != null && !truckDriverModel.consolidationDetails.getContainersList().isEmpty())
        {
            truckDriverModel.consolidationDetails.setContainersList(getConsolidationContainers(truckDriverModel.consolidationDetails.getContainersList(), 2));
        }
        if(truckDriverModel.consolidationDetails != null) {
            List<ShipmentModel> shipments = truckDriverModel.consolidationDetails.getShipmentsList();

            if (shipments != null && !shipments.isEmpty()) {
                int sum = shipments.stream().filter(c -> c.getNoOfPacks() != null).mapToInt(ShipmentModel::getNoOfPacks).sum();
                truckDriverModel.totalPacks += sum;
            }
        }
        truckDriverModel.usersDto = UserContext.getUser();
        return truckDriverModel;
    }

    private List<ContainerModel> getConsolidationContainers(List<ContainerModel> containersList, int decimal) {
        List<ContainerModel> containerModelList = new ArrayList<>();
        for(var container: containersList)
        {
            ContainerModel containerModel = new ContainerModel();
            containerModel.setContainerNumber(container.getContainerNumber());
            containerModel.setSealNumber(container.getSealNumber());
            containerModel.setGrossWeight(getRoundedBigDecimal(container.getGrossWeight(),decimal, RoundingMode.HALF_UP));
            containerModel.setGrossWeightUnit(container.getGrossWeightUnit());
            containerModel.setGrossVolume(getRoundedBigDecimal(container.getGrossVolume(),decimal, RoundingMode.HALF_UP));
            containerModel.setGrossVolumeUnit(container.getGrossVolumeUnit());
            containerModel.setContainerCode(container.getContainerCode());
            containerModel.setContainerCount(container.getContainerCount());
            containerModel.setNetWeight(getRoundedBigDecimal(container.getNetWeight(),decimal, RoundingMode.HALF_UP));
            containerModel.setNetWeightUnit(container.getNetWeightUnit());
            containerModel.setMinTemp(getRoundedBigDecimal(container.getMinTemp(),decimal, RoundingMode.HALF_UP));
            containerModel.setMinTempUnit(container.getMinTempUnit());
            containerModel.setDescriptionOfGoods(container.getDescriptionOfGoods());
            containerModel.setCarrierSealNumber(container.getCarrierSealNumber());
            containerModel.setCustomsSealNumber(container.getCustomsSealNumber());
            containerModelList.add(containerModel);
        }
        return containerModelList;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        TruckDriverModel truckDriverModel = (TruckDriverModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(truckDriverModel.consolidationDetails, GetDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateConsolidationFields(truckDriverModel.consolidationDetails, dictionary);
        populateUserFields(truckDriverModel.usersDto, dictionary);
        dictionary.put(ReportConstants.TOTAL_PACKS, truckDriverModel.totalPacks);
        return dictionary;
    }
}
