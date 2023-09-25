package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ManifestConsolModel;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.helper.ICarrierMasterData;
import com.nimbusds.jose.util.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        ConsolidationDetails consolidationDetails = getConsolidation(id);
        List<Containers> containersList = new ArrayList<>();
        if(consolidationDetails.getShipmentsList() != null) {
            for (var shipment : consolidationDetails.getShipmentsList()) {
                containersList.addAll(shipment.getContainersList());
            }
        }

        return ManifestConsolModel.builder()
            .consolidation(consolidationDetails)
            .containersList(jsonHelper.convertValueToList(containersList, ShipmentContainers.class))
            .containerCount(containersList.size())
            .shipmentDetailsList(consolidationDetails.getShipmentsList())
            .shipmentCount(consolidationDetails.getShipmentsList().size())
            .carrierMasterData(carrierMasterData.fetchCarrierData().get(0)) // ?
            .build();
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ManifestConsolModel model = (ManifestConsolModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();

        populateConsolidationFields(model.getConsolidation(), dictionary);
        List<Packing> packingList = GetAllShipmentsPacks(model.getShipmentDetailsList());
        Pair<BigDecimal, String> weightAndUnit = GetTotalWeight(packingList);
        Pair<BigDecimal, String> volumeAndUnit = GetTotalVolume(packingList);

        int totalPacks = 0;
        List<String> allPacksTypes = new ArrayList<>();

        dictionary.put(SHIPMENT_AND_CONTAINER, getShipmentAndContainerResponse(model.getShipmentDetailsList()));
        dictionary.put(SHIPMENTS, getShipmentResponse(model.getShipmentDetailsList()));
        dictionary.put(CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(model.getContainersList()));

        if (model.getShipmentDetailsList() != null) {
            dictionary.put(OBJECT_TYPE, model.getShipmentDetailsList().get(0).getTransportMode());
        }

        dictionary.put(CONTAINER_COUNT, model.getContainerCount());
        dictionary.put(SHIPMENT_COUNT, model.getShipmentCount());

        dictionary.put(CONSOL_CARRIER, model.getCarrierMasterData().getItemDescription());

        if (weightAndUnit.getLeft().compareTo(BigDecimal.ZERO) > 0)
            dictionary.put(TOTAL_WEIGHT, weightAndUnit.getLeft());
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
            dictionary.put(TOTAL_VOLUME, volumeAndUnit.getLeft());
        else
            dictionary.put(TOTAL_VOLUME, "-");

        dictionary.put(TOTAL_VOLUME_UNIT, volumeAndUnit.getRight());

        return dictionary;
    }
}
