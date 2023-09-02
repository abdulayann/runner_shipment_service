package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class ShipmentSync implements IShipmentSync {

    @Autowired
    ModelMapper modelMapper;

    @Override
    public CustomShipmentRequest sync(ShipmentDetails sd) {
        CustomShipmentRequest cs = new CustomShipmentRequest();

        // First map nested entity that are root level properties in v1
        mapAdditionalDetails(cs, sd);
        mapCarrierDetails(cs, sd);
        // Map remaining object so there's no info lost for root -> root properties
        // example Guid
        cs = modelMapper.map(sd, CustomShipmentRequest.class);

        // assigning root level properties not previously mapped
        cs.setCustom_ShipType(sd.getDirection());
        cs.setContainerType(sd.getShipmentType());
        cs.setStatusString(sd.getStatus().toString());
        cs.setSalesAgentId(sd.getSalesAgent());
        cs.setInners(sd.getInnerPacks());
        cs.setInnersUnit(sd.getInnerPackUnit());
        cs.setMarksnNums(sd.getMarksNum());
        cs.setConsolidationReferenceNumber(sd.getConsolRef());
        cs.setChargeable(sd.getChargable());
        cs.setChargableUnit(sd.getChargeableUnit());
        cs.setPacks(sd.getNoOfPacks());

        cs.setLockedByUser(sd.getLockedBy()); // lockedBy also present

        cs.setFinanceClosedByUser(sd.getFinanceClosedBy());

        // assigning child entities not automatically mapped
        // entityID also gets assigned as a part of this mapping
        mapTruckDriverDetail(cs, sd);
        cs.setRoutings(convertToList(sd.getRoutingsList(), RoutingsRequestV2.class));
        cs.setReferenceNumbers(convertToList(sd.getReferenceNumbersList(), ReferenceNumbersRequestV2.class));
        cs.setPackings(convertToList(sd.getPackingList(), PackingRequestV2.class));
        cs.setDocs(convertToList(sd.getFileRepoList(), FileRepoRequestV2.class));
        cs.setELDetails(convertToList(sd.getElDetailsList(), ElDetailsRequestV2.class));

        // Container missing mappings
        // dgClassString, IsHazardous, MarksnNums
        // PickupAddressJSON and DeliveryAddressJSON (could be renamed for easy mapping)

        cs.setBookingCarriages(convertToList(sd.getBookingCarriagesList(), BookingCarriageRequestV2.class));

        return cs;
    }
    
    public ShipmentDetails reverseSync(CustomShipmentRequest cs) {
        ShipmentDetails sd = new ShipmentDetails();
        sd.setCarrierDetails(modelMapper.map(cs, CarrierDetails.class));
        sd.setAdditionalDetails(modelMapper.map(cs, AdditionalDetails.class));

        sd.setDirection(cs.getCustom_ShipType());
        sd.setShipmentType(cs.getContainerType());
        sd.setStatus(Integer.parseInt(cs.getStatusString()));
        sd.setSalesAgent(cs.getSalesAgentId());
        sd.setInnerPacks(cs.getInners());
        sd.setInnerPackUnit(cs.getInnersUnit());
        sd.setMarksNum(cs.getMarksnNums());
        sd.setConsolRef(cs.getConsolidationReferenceNumber());
        sd.setChargable(cs.getChargeable());
        sd.setChargeableUnit(cs.getChargableUnit());
        sd.setNoOfPacks(cs.getPacks());

        sd.setFinanceClosedBy(cs.getFinanceClosedByUser());

        mapTruckDriverDetailReverse(cs, sd);
        sd.setRoutingsList(convertToList(cs.getRoutings(), Routings.class));
        sd.setReferenceNumbersList(convertToList(cs.getReferenceNumbers(), ReferenceNumbers.class));
        sd.setPackingList(convertToList(cs.getPackings(), Packing.class));
        sd.setFileRepoList(convertToList(cs.getDocs(), FileRepo.class));
        sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

        sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));

        return sd;
    }


    private void mapTruckDriverDetail(CustomShipmentRequest cs, ShipmentDetails sd) {
        if(sd.getTruckDriverDetails() == null)
            return;

        List<TruckDriverDetailsRequestV2> req = sd.getTruckDriverDetails().stream()
                .map(item -> {
                    TruckDriverDetailsRequestV2 t;
                    t = modelMapper.map(item, TruckDriverDetailsRequestV2.class);
                    t.setTransporterNameOrg(item.getTransporterName());
                    //ENUM
                    t.setTransporterTypeString(item.getTransporterType().toString());
                    return t;
                })
                .collect(Collectors.toList());

        cs.setTruckDriverDetail(req);

    }

    private void mapTruckDriverDetailReverse(CustomShipmentRequest cs, ShipmentDetails sd) {
        if(cs.getTruckDriverDetail() == null)
            return;

        List<TruckDriverDetails> req = cs.getTruckDriverDetail().stream()
                .map(item -> {
                    TruckDriverDetails t;
                    t = modelMapper.map(item, TruckDriverDetails.class);
                    t.setTransporterName(item.getTransporterNameOrg());
                    t.setTransporterType(Ownership.valueOf(item.getTransporterTypeString()));
                    return t;
                })
                .toList();
        sd.setTruckDriverDetails(req);
    }

    private void mapCarrierDetails(CustomShipmentRequest cs, ShipmentDetails sd) {
        if(sd.getCarrierDetails() == null)
            return;
        modelMapper.map(sd.getCarrierDetails(), cs);
    }

    private void mapAdditionalDetails(CustomShipmentRequest cs, ShipmentDetails sd) {
        if(sd.getAdditionalDetails() == null)
            return;
        modelMapper.map(sd.getAdditionalDetails(), cs);
    }

    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }
    private  <T,P> P convertToClass(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }
}
