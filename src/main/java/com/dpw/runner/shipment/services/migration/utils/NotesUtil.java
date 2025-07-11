package com.dpw.runner.shipment.services.migration.utils;

import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

@Slf4j
@Component
public class NotesUtil {

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IShipmentService shipmentService;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IShipmentDao shipmentDao;

    public void addNotesForShipment(ShipmentDetails shipmentDetails){
        Set<Containers> containersList = shipmentDetails.getContainersList();
        StringBuilder text = new StringBuilder();

        ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        Map<String, Object> masterDataResponse = shipmentService.fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);

        text.append("Please note that the Cargo details have been amended since it is a V2 shipment opened in V3.");
        text.append(nullSafe(shipmentDetails.getShipmentId())).append("\n");

        text.append("Total Gross Weight: ").append(nullSafe(shipmentDetails.getWeight())).append(" ").append(nullSafe(shipmentDetails.getWeightUnit())).append("\n");
        text.append("Net Weight: ").append(nullSafe(shipmentDetails.getNetWeight())).append(" ").append(nullSafe(shipmentDetails.getNetWeightUnit())).append("\n");
        text.append("Total Gross Volume: ").append(nullSafe(shipmentDetails.getVolume())).append(" ").append(nullSafe(shipmentDetails.getVolumeUnit())).append("\n");
        text.append("Total Weight Volume: ").append(nullSafe(shipmentDetails.getVolumetricWeight())).append(" ").append(nullSafe(shipmentDetails.getVolumetricWeightUnit())).append("\n");
        text.append("Chargeable Weight: ").append(nullSafe(shipmentDetails.getChargable())).append(" ").append(nullSafe(shipmentDetails.getChargeableUnit())).append("\n");
        text.append("No of packages: ").append(nullSafe(shipmentDetails.getNoOfPacks())).append(" ").append(nullSafe(shipmentDetails.getPacksUnit())).append("\n");
        text.append("Commodity: ").append(nullSafe(shipmentDetails.getCommodity())).append("\n");

        if (containersList != null && !containersList.isEmpty()) {
            log.info("containerList: {}", containersList);
            Map<String, String> containerTypesMasterData = safeCastMap(masterDataResponse.get("ContainerTypes"));
            Map<String, String> commodityMasterData = safeCastMap(masterDataResponse.get("Commodity"));
            Map<String, Object> masterLists = safeCastMapObject(masterDataResponse.get("MasterLists"));
            Map<String, String> dgClassMasterData = safeCastMap(masterLists.get("DG_CLASS"));
            Map<String, String> packingGroupMasterData = safeCastMap(masterLists.get("PACKING_GROUP"));
            Map<String, String> packUnitMasterData = safeCastMap(masterLists.get("PACKS_UNIT"));
            Map<String, String> unLocationMasterData = safeCastMap(masterDataResponse.get("Unlocations"));
            appendContainerDataInText(containersList, text, containerTypesMasterData, commodityMasterData, dgClassMasterData, packingGroupMasterData, packUnitMasterData, unLocationMasterData);
        }

        NotesRequest notesRequest = NotesRequest.builder()
                .entityId(shipmentDetails.getId())
                .entityType("SHIPMENT")
                .text(text.toString())
                .build();

        Notes notes = jsonHelper.convertValue(notesRequest, Notes.class);
        notesDao.save(notes);
    }

    private void appendContainerDataInText(Set<Containers> containersList, StringBuilder text, Map<String, String> containerTypesMasterData, Map<String, String> commodityMasterData, Map<String, String> dgClassMasterData, Map<String, String> packingGroupMasterData, Map<String, String> packUnitMasterData, Map<String, String> unLocationMasterData) {
        for (Containers container : containersList) {
            text.append("Container Details\n");
            text.append(nullSafe(container.getContainerNumber())).append("\n");
            text.append("Container Type: ").append(getMasterValue(containerTypesMasterData, container.getContainerCode())).append("\n");
            text.append("Container Number: ").append(nullSafe(container.getContainerNumber())).append("\n");
            text.append("Container Count: ").append(nullSafe(container.getContainerCount())).append("\n");
            text.append("Description of Goods: ").append(nullSafe(container.getDescriptionOfGoods())).append("\n");
            text.append("Marks and Number: ").append(nullSafe(container.getMarksNums())).append("\n");
            text.append("Container Comments: ").append(nullSafe(container.getContainerComments())).append("\n");
            text.append("Handling Information: ").append(nullSafe(container.getHandlingInfo())).append("\n");
            text.append("Commodity Category: ").append(nullSafe(container.getCommodityGroup())).append("\n");
            text.append("Commodity: ").append(getMasterValue(commodityMasterData, container.getCommodityCode())).append("\n");
            text.append("HS Code: ").append(nullSafe(container.getHsCode())).append("\n");
            text.append("Gross Weight: ").append(nullSafe(container.getGrossWeight())).append(" ").append(nullSafe(container.getGrossWeightUnit())).append("\n");
            text.append("Net Weight: ").append(nullSafe(container.getNetWeight())).append(" ").append(nullSafe(container.getNetWeightUnit())).append("\n");
            text.append("Tare Weight: ").append(nullSafe(container.getTareWeight())).append(" ").append(nullSafe(container.getTareWeightUnit())).append("\n");
            text.append("Gross Volume: ").append(nullSafe(container.getGrossVolume())).append(" ").append(nullSafe(container.getGrossVolumeUnit())).append("\n");
            text.append("Dangerous Goods: ").append(Boolean.TRUE.equals(container.getHazardous())).append("\n");
            text.append("DG Class: ").append(getMasterValue(dgClassMasterData, container.getDgClass())).append("\n");
            text.append("UN Number: ").append(nullSafe(container.getUnNumber())).append("\n");
            text.append("Proper Shipping Name: ").append(nullSafe(container.getProperShippingName())).append("\n");
            text.append("Packing Group: ").append(getMasterValue(packingGroupMasterData, container.getPackingGroup())).append("\n");
            text.append("Minimum Flash: ").append(nullSafe(container.getMinimumFlashPoint())).append(" ").append(nullSafe(container.getMinimumFlashPointUnit())).append("\n");
            text.append("Marine Pollutant: ").append(Boolean.TRUE.equals(container.getMarinePollutant())).append("\n");
            text.append("No Of packages: ").append(nullSafe(container.getPacks())).append("\n");
            text.append("Package Type: ").append(getMasterValue(packUnitMasterData, container.getPacksType())).append("\n");
            text.append("Container Temperature: ").append(nullSafe(container.getMinTemp())).append(" ").append(nullSafe(container.getMinTempUnit())).append("\n");
            text.append("Customs Release Code: ").append(nullSafe(container.getCustomsReleaseCode())).append("\n");
            text.append("PACR Number: ").append(nullSafe(container.getPacrNumber())).append("\n");
            text.append("Container Stuffing Location: ").append(getMasterValue(unLocationMasterData, container.getContainerStuffingLocation())).append("\n");
            text.append("Container Invoice Number: ").append(nullSafe(container.getInvoiceNumber())).append("\n");
            text.append("Container Invoice Value: ").append(nullSafe(container.getInvoiceValue())).append(" ").append(nullSafe(container.getInvoiceCurrency())).append("\n");
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, String> safeCastMap(Object obj) {
        try {
            return obj != null ? (Map<String, String>) obj : Collections.emptyMap();
        } catch (ClassCastException e) {
            return Collections.emptyMap();
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> safeCastMapObject(Object obj) {
        try {
            return obj != null ? (Map<String, Object>) obj : Collections.emptyMap();
        } catch (ClassCastException e) {
            return Collections.emptyMap();
        }
    }

    private String getMasterValue(Map<String, String> masterData, String key) {
        return (masterData != null && key != null) ? masterData.getOrDefault(key, "N/A") : "N/A";
    }

    private String nullSafe(Object obj) {
        return obj != null ? obj.toString() : "";
    }


}
