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
        if(shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            text.append("Volume Weight: ").append(nullSafe(shipmentDetails.getVolumetricWeight())).append(" ").append(nullSafe(shipmentDetails.getVolumetricWeightUnit())).append("\n");
        else
            text.append("Weight Volume: ").append(nullSafe(shipmentDetails.getVolumetricWeight())).append(" ").append(nullSafe(shipmentDetails.getVolumetricWeightUnit())).append("\n");

        text.append("Chargeable Weight: ").append(nullSafe(shipmentDetails.getChargable())).append(" ").append(nullSafe(shipmentDetails.getChargeableUnit())).append("\n");
        text.append("No of packages: ").append(nullSafe(shipmentDetails.getNoOfPacks())).append(" ").append(nullSafe(shipmentDetails.getPacksUnit())).append("\n");
        text.append("\n");

        appendContainerDataInText(containersList, text);
        appendPackingDataInText(packingList, text);
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
        for (Containers container : containersList) {
            text.append("Container Summary\n");
            text.append(nullSafe(container.getContainerNumber())).append("\n");
            text.append("Container Type: ").append(container.getContainerCode()).append("\n");
            text.append("Container Number: ").append(nullSafe(container.getContainerNumber())).append("\n");
            text.append("No Of packages: ").append(nullSafe(container.getPacks())).append("\n");
            text.append("Package Type: ").append(container.getPacksType()).append("\n");
            text.append("Gross Weight: ").append(nullSafe(container.getGrossWeight())).append(" ").append(nullSafe(container.getGrossWeightUnit())).append("\n");
            text.append(netWeight).append(nullSafe(container.getNetWeight())).append(" ").append(nullSafe(container.getNetWeightUnit())).append("\n");
            text.append("Tare Weight: ").append(nullSafe(container.getTareWeight())).append(" ").append(nullSafe(container.getTareWeightUnit())).append("\n");
            text.append("Gross Volume: ").append(nullSafe(container.getGrossVolume())).append(" ").append(nullSafe(container.getGrossVolumeUnit())).append("\n");
            text.append("\n");
        }
    }

    private void appendPackingDataInText(List<Packing> packingList, StringBuilder text) {
        if (packingList == null || packingList.isEmpty()) {
            return;
        }
        log.info("packingList: {}", packingList);
        text.append("Packages# ").append("\n");
        for(Packing packing: packingList){
            text.append("No Of Package: ").append(nullSafe(packing.getPacks())).append(" ").append(nullSafe(packing.getPacksType())).append("\n");
            text.append("Width: ").append(nullSafe(packing.getWidth())).append(" ").append(nullSafe(packing.getWidthUnit())).append("\n");
            text.append("Height: ").append(nullSafe(packing.getHeight())).append(" ").append(nullSafe(packing.getHeightUnit())).append("\n");
            text.append("Gross Weight: ").append(nullSafe(packing.getWeight())).append(" ").append(nullSafe(packing.getWeightUnit())).append("\n");
            text.append(netWeight).append(nullSafe(packing.getNetWeight())).append(" ").append(nullSafe(packing.getNetWeightUnit())).append("\n");
            text.append("Gross Volume: ").append(nullSafe(packing.getVolume())).append(" ").append(nullSafe(packing.getVolumeUnit())).append("\n");
            text.append("Volume Weight: ").append(nullSafe(packing.getVolumeWeight())).append(nullSafe(packing.getVolumeWeightUnit())).append("\n");
            text.append("Chargeable: ").append(nullSafe(packing.getChargeable())).append(nullSafe(packing.getChargeableUnit())).append("\n");
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
        appendPackingDataInText(packingList, text);
        return text;
    }

    private String nullSafe(Object obj) {
        return obj != null ? obj.toString() : "";
    }


}
