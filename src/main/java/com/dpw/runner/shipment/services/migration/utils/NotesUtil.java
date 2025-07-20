package com.dpw.runner.shipment.services.migration.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class NotesUtil {

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IShipmentService shipmentService;

    @Autowired
    private IPackingV3Service packingV3Service;

    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IShipmentDao shipmentDao;

    String netWeight = "Net Weight: ";
    String commodity = "Commodity: ";

    public void addNotesForShipment(ShipmentDetails shipmentDetails){
        StringBuilder text = getShipmentNotes(shipmentDetails);
        Notes notes = getNotes(shipmentDetails.getId(), "SHIPMENT", text);
        notesDao.save(notes);
    }

    public StringBuilder getShipmentNotes(ShipmentDetails shipmentDetails) {
        Set<Containers> containersList = shipmentDetails.getContainersList();
        List<Packing> packingList = shipmentDetails.getPackingList();
        StringBuilder text = new StringBuilder();

        text.append("Please note that the Cargo details have been amended since it is a V2 shipment opened in V3.").append("\n");
        text.append(nullSafe(shipmentDetails.getShipmentId())).append("\n");

        text.append("Total Gross Weight: ").append(nullSafe(shipmentDetails.getWeight())).append(" ").append(nullSafe(shipmentDetails.getWeightUnit())).append("\n");
        text.append(netWeight).append(nullSafe(shipmentDetails.getNetWeight())).append(" ").append(nullSafe(shipmentDetails.getNetWeightUnit())).append("\n");
        text.append("Total Gross Volume: ").append(nullSafe(shipmentDetails.getVolume())).append(" ").append(nullSafe(shipmentDetails.getVolumeUnit())).append("\n");
        text.append("Total Weight Volume: ").append(nullSafe(shipmentDetails.getVolumetricWeight())).append(" ").append(nullSafe(shipmentDetails.getVolumetricWeightUnit())).append("\n");
        text.append("Chargeable Weight: ").append(nullSafe(shipmentDetails.getChargable())).append(" ").append(nullSafe(shipmentDetails.getChargeableUnit())).append("\n");
        text.append("No of packages: ").append(nullSafe(shipmentDetails.getNoOfPacks())).append(" ").append(nullSafe(shipmentDetails.getPacksUnit())).append("\n");
        text.append(commodity).append(nullSafe(shipmentDetails.getCommodity())).append("\n");
        text.append("\n");

        appendContainerDataInText(containersList, text);
        appendPackingDataInText(packingList, containersList, text);
        return text;
    }

    public Notes getNotes(Long entityId, String entityType, StringBuilder text) {
        NotesRequest notesRequest = NotesRequest.builder().entityId(entityId)
                .entityType(entityType).text(text.toString()).build();

        Notes notes = jsonHelper.convertValue(notesRequest, Notes.class);
        notes.setIsReadOnly(Boolean.TRUE);
        notes.setCreatedBy(Constants.APPLICATION);
        notes.setUpdatedBy(Constants.APPLICATION);
        return notes;
    }

    private void appendContainerDataInText(Set<Containers> containersList, StringBuilder text) {
        if (containersList == null || containersList.isEmpty()) {
            return;
        }
        log.info("containerList: {}", containersList);
        List<ContainerBaseResponse> containerResponsesList = new ArrayList<>();
        for(Containers container : containersList){
            containerResponsesList.add(jsonHelper.convertValue(container, ContainerBaseResponse.class));
        }
        Map<String, Object> masterDataResponse = containerV3Service.getMasterDataForList(containerResponsesList, true);
        Map<String, String> containerTypesMasterData = safeCastMap(masterDataResponse.get("ContainerTypes"));
        Map<String, String> commodityMasterData = safeCastMap(masterDataResponse.get("Commodity"));
        Map<String, Object> masterLists = safeCastMapObject(masterDataResponse.get("MasterLists"));
        Map<String, String> dgClassMasterData = safeCastMap(masterLists.get("DG_CLASS"));
        Map<String, String> packingGroupMasterData = safeCastMap(masterLists.get("PACKING_GROUP"));
        Map<String, String> packUnitMasterData = safeCastMap(masterLists.get("PACKS_UNIT"));
        Map<String, String> unLocationMasterData = safeCastMap(masterDataResponse.get("Unlocations"));
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
            text.append(commodity).append(getMasterValue(commodityMasterData, container.getCommodityCode())).append("\n");
            text.append("HS Code: ").append(nullSafe(container.getHsCode())).append("\n");
            text.append("Gross Weight: ").append(nullSafe(container.getGrossWeight())).append(" ").append(nullSafe(container.getGrossWeightUnit())).append("\n");
            text.append(netWeight).append(nullSafe(container.getNetWeight())).append(" ").append(nullSafe(container.getNetWeightUnit())).append("\n");
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
            text.append("\n");
        }
    }

    private void appendPackingDataInText(List<Packing> packingList, Set<Containers> containersSet,StringBuilder text) {
        if (packingList == null || packingList.isEmpty()) {
            return;
        }
        log.info("packingList: {}", packingList);
        List<PackingResponse> packingResponseList = new ArrayList<>();
        for(Packing packing : packingList){
            packingResponseList.add(jsonHelper.convertValue(packing, PackingResponse.class));
        }
        Map<String, Object> masterDataResponse = packingV3Service.getMasterDataForList(packingResponseList, true);
        Integer i = 1;

        Map<Long, Containers> packContainerMap = new HashMap<>();

        Map<Long, Containers> containerIdMap = new HashMap<>();
        if(containersSet!=null && !containersSet.isEmpty()) {
            for (Containers container : containersSet) {
                containerIdMap.put(container.getId(), container);
            }
        }

        for(Packing packing: packingList){
            Containers matchedContainer = containerIdMap.get(packing.getContainerId());
            if (matchedContainer != null) {
                packContainerMap.put(packing.getId(), matchedContainer);
            }
        }
        Map<String, String> unLocationMasterData = safeCastMap(masterDataResponse.get("Unlocations"));
        Map<String, String> commodityMasterData = safeCastMap(masterDataResponse.get("Commodity"));
        Map<String, Object> masterLists = safeCastMapObject(masterDataResponse.get("MasterLists"));
        Map<String, String> dgClassMasterData = safeCastMap(masterLists.get("DG_CLASS"));
        Map<String, String> packingGroupMasterData = safeCastMap(masterLists.get("PACKING_GROUP"));
        for(Packing packing: packingList){
            String containerNumber = packContainerMap.get(packing.getId())!=null ? packContainerMap.get(packing.getId()).getContainerNumber(): "";
            text.append("Package# ").append(i).append(":").append("\n");
            text.append("Container Number: ").append(nullSafe(containerNumber)).append("\n");
            text.append("Reference Number: ").append(nullSafe(packing.getReferenceNumber())).append("\n");
            text.append("No Of Package: ").append(nullSafe(packing.getPacks())).append(" ").append(nullSafe(packing.getPacksType())).append("\n");
            text.append("Packing Order: ").append(nullSafe(packing.getPackingOrder())).append("\n");
            text.append("Marks and Number: ").append(nullSafe(packing.getMarksnNums())).append("\n");
            text.append("Origin: ").append(nullSafe(getMasterValue(unLocationMasterData, packing.getOrigin()))).append("\n");
            text.append("Country Code: ").append(nullSafe(packing.getCountryCode())).append("\n");
            text.append("Commodity Category: ").append(nullSafe(packing.getCommodityGroup())).append("\n");
            text.append(commodity).append(nullSafe(getMasterValue(commodityMasterData, packing.getCommodity()))).append("\n");
            text.append("HS Code: ").append(nullSafe(packing.getHSCode())).append("\n");
            text.append("Length: ").append(nullSafe(packing.getLength())).append(" ").append(nullSafe(packing.getLengthUnit())).append("\n");
            text.append("Width: ").append(nullSafe(packing.getWidth())).append(" ").append(nullSafe(packing.getWidthUnit())).append("\n");
            text.append("Height: ").append(nullSafe(packing.getHeight())).append(" ").append(nullSafe(packing.getHeightUnit())).append("\n");
            text.append("Gross Weight: ").append(nullSafe(packing.getWeight())).append(" ").append(nullSafe(packing.getWeightUnit())).append("\n");
            text.append(netWeight).append(nullSafe(packing.getNetWeight())).append(" ").append(nullSafe(packing.getNetWeightUnit())).append("\n");
            text.append("Gross Volume: ").append(nullSafe(packing.getVolume())).append(" ").append(nullSafe(packing.getVolumeUnit())).append("\n");
            text.append("Weight Volume: ").append(nullSafe(packing.getVolumeWeight())).append(nullSafe(packing.getVolumeWeightUnit())).append("\n");
            text.append("Goods Description: ").append(nullSafe(packing.getGoodsDescription())).append("\n");
            text.append("Handling Information: ").append(nullSafe(packing.getHandlingInfo())).append("\n");
            text.append("Inspections: ").append(nullSafe(packing.getInspections())).append("\n");
            text.append("Dangerous Goods: ").append(nullSafe(Boolean.TRUE.equals(packing.getHazardous()))).append("\n");
            text.append("DG Class: ").append(nullSafe(getMasterValue(dgClassMasterData, packing.getDGClass()))).append("\n");
            text.append("UN Number: ").append(nullSafe(packing.getUnNumber())).append("\n");
            text.append("Proper Shipping Name: ").append(nullSafe(packing.getProperShippingName())).append("\n");
            text.append("Packing Group: ").append(getMasterValue(packingGroupMasterData, packing.getPackingGroup())).append("\n");
            text.append("Minimum Flash: ").append(nullSafe(packing.getMinimumFlashPoint())).append(" ").append(nullSafe(packing.getMinimumFlashPointUnit())).append("\n");
            text.append("Marine Pollutant: ").append(Boolean.TRUE.equals(packing.getMarinePollutant())).append("\n");
            text.append("Temperature Controlled: ").append(Boolean.TRUE.equals(packing.getIsTemperatureControlled())).append("\n");
            text.append("\n");
        }
    }

    public void addNotesForConsolidation(ConsolidationDetails consolidationDetails){
        StringBuilder text = getConsolNotes(consolidationDetails);
        Notes notes = getNotes(consolidationDetails.getId(), "CONSOLIDATION", text);
        notesDao.save(notes);
    }

    public StringBuilder getConsolNotes(ConsolidationDetails consolidationDetails) {
        List<Containers> containersList = consolidationDetails.getContainersList();
        List<Packing> packingList = consolidationDetails.getPackingList();
        Set<Containers> containersSet = new HashSet<>(containersList);
        StringBuilder text = new StringBuilder();
        Set<ShipmentDetails> shipmentDetailsList = consolidationDetails.getShipmentsList();
        for(ShipmentDetails shipmentDetails: shipmentDetailsList){
            addNotesForShipment(shipmentDetails);
        }

        text.append("Please note that the Cargo details have been amended since it is a V2 consolidation opened in V3.");
        text.append(nullSafe(consolidationDetails.getConsolidationNumber())).append("\n");

        appendContainerDataInText(containersSet, text);
        appendPackingDataInText(packingList, containersSet, text);
        return text;
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
