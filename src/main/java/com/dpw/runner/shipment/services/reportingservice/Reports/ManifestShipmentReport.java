package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ManifestShipmentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.nimbusds.jose.util.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Component
public class ManifestShipmentReport extends IReport{
    @Autowired
    private JsonHelper jsonHelper;
    @Override
    public Map<String, Object> getData(Long id) {
        ManifestShipmentModel manifestShipmentModel = (ManifestShipmentModel) getDocumentModel(id);
        return populateDictionary(manifestShipmentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {

        ManifestShipmentModel manifestShipmentModel = new ManifestShipmentModel();
        manifestShipmentModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(manifestShipmentModel.shipmentDetails);
        if(manifestShipmentModel.shipmentDetails != null && manifestShipmentModel.shipmentDetails.getConsolidationList() != null && !manifestShipmentModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            manifestShipmentModel.consolidationDetails = manifestShipmentModel.shipmentDetails.getConsolidationList().get(0);
        }
        manifestShipmentModel.setContainers(new ArrayList<>());
        if(manifestShipmentModel.shipmentDetails != null && manifestShipmentModel.shipmentDetails.getContainersList() != null)
        {
            for(ContainerModel container : manifestShipmentModel.shipmentDetails.getContainersList())
                manifestShipmentModel.getContainers().add(getShipmentContainer(container));
        }
        if(manifestShipmentModel.shipmentDetails != null && manifestShipmentModel.shipmentDetails.getCarrierDetails() != null) {
            manifestShipmentModel.carrier = getCarrier(manifestShipmentModel.shipmentDetails.getCarrierDetails().getShippingLine());
        }
        return manifestShipmentModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ManifestShipmentModel manifestShipmentModel = (ManifestShipmentModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateShipmentFields(manifestShipmentModel.shipmentDetails, dictionary);
        populateConsolidationFields(manifestShipmentModel.consolidationDetails, dictionary);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();

        List<PackingModel> packings = GetAllShipmentsPacks(List.of(manifestShipmentModel.shipmentDetails));
        Pair<BigDecimal, String> weightAndUnit = GetTotalWeight(packings);
        Pair<BigDecimal, String> volumeAndUnit = GetTotalVolume(packings);

        if (manifestShipmentModel.shipmentDetails != null) {
            dictionary.put(ReportConstants.OBJECT_TYPE, manifestShipmentModel.shipmentDetails.getTransportMode());
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(manifestShipmentModel.getContainers()));

        dictionary.put(ReportConstants.SHIPMENT_AND_CONTAINER, getShipmentAndContainerResponse(List.of(manifestShipmentModel.shipmentDetails)));
        var listShipmentReponse = getShipmentResponse(List.of(manifestShipmentModel.shipmentDetails));
        dictionary.put(ReportConstants.SHIPMENTS, listShipmentReponse);

        if (listShipmentReponse != null) {
            List<Map<String, Object>> values = jsonHelper.convertValue(listShipmentReponse, new TypeReference<>() {});
            values.forEach(v -> {
                if (v.containsKey(ReportConstants.WEIGHT))
                    v.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.WEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.TOTAL_PACKS))
                    v.put(ReportConstants.TOTAL_PACKS, addCommas(v.get(ReportConstants.TOTAL_PACKS).toString()));
            });
            dictionary.put(ReportConstants.SHIPMENTS, values);
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT, manifestShipmentModel.shipmentDetails.getContainersList().size());
        if (manifestShipmentModel.carrier != null) {
            dictionary.put(ReportConstants.CARRIER_NAME, manifestShipmentModel.carrier.getItemDescription());
            dictionary.put(ReportConstants.FLIGHT_CARRIER, manifestShipmentModel.carrier.getItemDescription());
        }

        if (packings != null && !packings.isEmpty()) {
            AtomicInteger totalPacks = new AtomicInteger();
            Set<String> allPackages = packings.stream().filter(x -> x.getPacksType() != null).map(PackingModel::getPacksType).collect(Collectors.toSet());
            packings.forEach(p -> {
                int number = Integer.parseInt(p.getPacks());
                totalPacks.addAndGet(number);
            });
            dictionary.put(ReportConstants.TOTAL_PACKS, addCommas(totalPacks.get()));
            dictionary.put(ReportConstants.TOTAL_PACKS_TYPE, allPackages);
        }
        dictionary.put(ReportConstants.TOTAL_PACKS_WEIGHT, addCommas(weightAndUnit.getLeft()));
        dictionary.put(ReportConstants.TOTAL_WEIGHT_UNIT, weightAndUnit.getRight());
        dictionary.put(ReportConstants.TOTAL_PACKS_VOLUME, addCommas(volumeAndUnit.getLeft()));
        dictionary.put(ReportConstants.TOTAL_VOLUME_UNIT, volumeAndUnit.getRight());
        return dictionary;
    }
}
