package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShippingRequestAirModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.*;

@Component
public class ShippingRequestAirReport extends IReport {
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IV1Service v1Service;

    @Override
    public Map<String, Object> getData(Long id) {
        ShippingRequestAirModel shippingRequestAirModel = (ShippingRequestAirModel) getDocumentModel(id);
        return populateDictionary(shippingRequestAirModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        ShippingRequestAirModel shippingRequestAirModel = new ShippingRequestAirModel();
        shippingRequestAirModel.shipment = getShipment(id);
        validateAirAndOceanDGCheck(shippingRequestAirModel.shipment);
        shippingRequestAirModel.setShipmentPacking(shippingRequestAirModel.shipment.getPackingList());
        return shippingRequestAirModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShippingRequestAirModel shippingRequestAirModel = (ShippingRequestAirModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateShipmentFields(shippingRequestAirModel.shipment, dictionary);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();

        List<Map<String, Object>> packDictionary = new ArrayList<>();
        List<PackingModel> listOfPacks = shippingRequestAirModel.getShipmentPacking();

        StringBuilder commodities = new StringBuilder();
        for (var pack : listOfPacks) {
            Map<String, Object> dict = new HashMap<>();
            dict.put(ReportConstants.LENGTH, pack.getLength());
            dict.put(ReportConstants.WIDTH, pack.getWidth());
            dict.put(ReportConstants.HEIGHT, pack.getHeight());
            dict.put(ReportConstants.PACK_COUNT, pack.getPacks());
            dict.put(ReportConstants.GROSS_WEIGHT, ConvertToWeightNumberFormat(pack.getWeight(), v1TenantSettingsResponse));
            dict.put(ReportConstants.VOLUME, ConvertToVolumeNumberFormat(pack.getVolume(), v1TenantSettingsResponse));

            packDictionary.add(dict);

            if (pack.getCommodity() != null) {

                List<Object> criteria = Arrays.asList(
                        List.of("Code"),
                        "=",
                        pack.getCommodity()
                );
                CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
                V1DataResponse response = v1Service.fetchCommodityData(commonV1ListRequest);

                List<EntityTransferCommodityType> commodityTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);

                if (commodityTypeList != null && !commodityTypeList.isEmpty() &&
                        commodityTypeList.get(0).getDescription() != null && !commodityTypeList.get(0).getDescription().isEmpty()) {
                    if (!commodities.toString().equals(""))
                        commodities.append(",");
                    commodities.append(commodityTypeList.get(0).getDescription());
                }

            }
        }
        dictionary.put(ReportConstants.LIST_OF_PACKINGS, packDictionary);
        dictionary.put(ReportConstants.COMMODITIES, commodities);
        dictionary.put(ReportConstants.PRINT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));

        return dictionary;
    }
}
