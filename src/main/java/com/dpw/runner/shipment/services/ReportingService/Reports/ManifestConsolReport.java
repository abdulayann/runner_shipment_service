package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentAndContainerResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ManifestConsolModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.helper.ICarrierMasterData;
import com.fasterxml.jackson.core.type.TypeReference;
import com.nimbusds.jose.util.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class ManifestConsolReport extends IReport {

    @Autowired
    ICarrierMasterData carrierMasterData;
    @Autowired
    JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        ManifestConsolModel model = (ManifestConsolModel) getDocumentModel(id);
        return populateDictionary(model);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        ConsolidationModel consolidationDetails = getConsolidation(id);
        List<ContainerModel> containersList = new ArrayList<>();
        if(consolidationDetails.getShipmentsList() != null) {
            for (var shipment : consolidationDetails.getShipmentsList()) {
                containersList.addAll(shipment.getContainersList());
            }
        }

        ManifestConsolModel model =  ManifestConsolModel.builder()
            .consolidation(consolidationDetails)
            .containersList(jsonHelper.convertValueToList(containersList, ShipmentContainers.class))
            .containerCount(containersList.size())
            .shipmentDetailsList(consolidationDetails.getShipmentsList())
            .shipmentCount(consolidationDetails.getShipmentsList().size())
            .build();
        if(model.getConsolidation() != null && model.getConsolidation().getCarrierDetails() != null) {
            model.setCarrierMasterData(getCarrier(model.getConsolidation().getCarrierDetails().getShippingLine()));
        }

        return model;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ManifestConsolModel model = (ManifestConsolModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();

        populateConsolidationFields(model.getConsolidation() , dictionary);
        List<PackingModel> packingList = GetAllShipmentsPacks(model.getShipmentDetailsList());
        Pair<BigDecimal, String> weightAndUnit = GetTotalWeight(packingList);
        Pair<BigDecimal, String> volumeAndUnit = GetTotalVolume(packingList);

        int totalPacks = 0;
        List<String> allPacksTypes = new ArrayList<>();

        Pair<BigDecimal, String> totalWeightManifest = getTotalWeightManifest(model.getShipmentDetailsList());
        Pair<BigDecimal, String> totalVolumeManifest = getTotalVolumeManifest(model.getShipmentDetailsList());
        Pair<BigDecimal, String> totalPacksManifest = getTotalPacksManifest(model.getShipmentDetailsList());

        List<ShipmentAndContainerResponse> shipmentContainers = getShipmentAndContainerResponse(model.getShipmentDetailsList());
        if(shipmentContainers != null) {
            List<Map<String, Object>> values = new ArrayList<>();
            for (ShipmentAndContainerResponse shipmentContainers1 : shipmentContainers) {
                values.add(jsonHelper.convertValue(shipmentContainers1, new TypeReference<>() {}));
            }
            if (Objects.isNull(values)) values = new ArrayList<>();
            values.forEach(v -> {
                v.put(GROSS_WEIGHT, v.get(WEIGHT));
                v.put(GROSS_VOLUME, v.get(VOLUME));
                v.put(HSN_NUMBER, AmountNumberFormatter.formatWithoutDecimal(v.get(HSN_NUMBER), v.get(SHIPMENT_BILLCHARGES_FREIGHTOVERSEASCURRENCY) != null ? v.get(SHIPMENT_BILLCHARGES_FREIGHTOVERSEASCURRENCY).toString() : null, v1TenantSettingsResponse));
            });
            dictionary.put(SHIPMENT_AND_CONTAINER, values);
        }


        List<ShipmentResponse> shipments = getShipmentResponse(model.getShipmentDetailsList());
        if(shipments != null) {
            var values = shipments.stream()
                    .map(i -> jsonHelper.convertJsonToMap(jsonHelper.convertToJson(i)))
                    .toList();
            if (Objects.isNull(values)) values = new ArrayList<>();
            values.forEach(v -> {
                v.put(WEIGHT, ConvertToWeightNumberFormat(v.get(WEIGHT), v1TenantSettingsResponse));
                v.put(TOTAL_PACKS, AmountNumberFormatter.formatWithoutDecimal(v.get(TOTAL_PACKS), v.get(SHIPMENT_BILLCHARGES_FREIGHTOVERSEASCURRENCY) != null ? v.get(SHIPMENT_BILLCHARGES_FREIGHTOVERSEASCURRENCY).toString() : null, v1TenantSettingsResponse));
                v.put(HSN_NUMBER, AmountNumberFormatter.formatWithoutDecimal(v.get(HSN_NUMBER), v.get(SHIPMENT_BILLCHARGES_FREIGHTOVERSEASCURRENCY) != null ? v.get(SHIPMENT_BILLCHARGES_FREIGHTOVERSEASCURRENCY).toString() : null, v1TenantSettingsResponse));
            });
            dictionary.put(SHIPMENTS, values);
        }

        dictionary.put(CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(model.getContainersList()));

        if (model.getShipmentDetailsList() != null && model.getShipmentDetailsList().size() > 0) {
            dictionary.put(OBJECT_TYPE, model.getShipmentDetailsList().get(0).getTransportMode());
        }

        dictionary.put(CONTAINER_COUNT, model.getContainerCount());
        dictionary.put(SHIPMENT_COUNT, model.getShipmentCount());

        dictionary.put(CONSOL_CARRIER, model.getCarrierMasterData() != null ? model.getCarrierMasterData().getItemDescription() : null);

        if (weightAndUnit.getLeft().compareTo(BigDecimal.ZERO) > 0)
            dictionary.put(TOTAL_WEIGHT, ConvertToWeightNumberFormat(weightAndUnit.getLeft(), v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_WEIGHT, "-");

        dictionary.put(TOTAL_WEIGHT_UNIT, weightAndUnit.getRight());

        if(packingList != null)
        {
            for (var packing : packingList)
            {
                try{
                    totalPacks += Integer.parseInt(packing.getPacks());;
                } catch (Exception ignored){}

                if(!allPacksTypes.contains(packing.getPacksType()))
                    allPacksTypes.add(packing.getPacksType());
            }
        }

        dictionary.put(TOTAL_PACKS, totalPacks);
        dictionary.put(TOTAL_PACKS_TYPE, allPacksTypes.size() > 0 ? String.join(",", allPacksTypes) : "");

        if (volumeAndUnit.getLeft().compareTo(BigDecimal.ZERO) > 0)
            dictionary.put(TOTAL_VOLUME, ConvertToVolumeNumberFormat(volumeAndUnit.getLeft(), v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_VOLUME, "-");

        dictionary.put(TOTAL_VOLUME_UNIT, volumeAndUnit.getRight());

        if(totalWeightManifest.getLeft().compareTo(BigDecimal.ZERO) > 0)
            dictionary.put(TOTAL_WEIGHT_MANIFEST, ConvertToWeightNumberFormat(totalWeightManifest.getLeft(), v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_WEIGHT_MANIFEST, "-");
        dictionary.put(TOTAL_WEIGHT_UNIT_MANIFEST, totalWeightManifest.getRight());

        if(totalVolumeManifest.getLeft().compareTo(BigDecimal.ZERO) > 0)
            dictionary.put(TOTAL_VOLUME_MANIFEST, ConvertToVolumeNumberFormat(totalVolumeManifest.getLeft(), v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_VOLUME_MANIFEST, "-");
        dictionary.put(TOTAL_VOLUME_UNIT_MANIFEST, totalVolumeManifest.getRight());

        if(totalPacksManifest.getLeft().compareTo(BigDecimal.ZERO) > 0)
            dictionary.put(TOTAL_PACKS_MANIFEST, AmountNumberFormatter.formatWithoutDecimal(totalPacksManifest.getLeft(), model.getShipmentDetailsList().get(0).getFreightOverseasCurrency(), v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_PACKS_MANIFEST, "-");
        dictionary.put(TOTAL_PACKS_TYPE_MANIFEST, totalPacksManifest.getRight());

        dictionary.put(TOTAL_MBL, 1);
        dictionary.put(TOTAL_MAWB, 1);

        return dictionary;
    }
}
