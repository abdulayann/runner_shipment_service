package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
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


        mapAdditionalDetails(cs, sd);
        mapCarrierDetails(cs, sd);

        // assigning child entities not automatically mapped
        // entityID also gets assigned as a part of this mapping

        mapTruckDriverDetail(cs, sd);
        //Routing IsDomestic <- domestic (no mapped via mapper)
        cs.setRoutings(convertToList(sd.getRoutingsList(), RoutingsRequestV2.class));
        cs.setReferenceNumbers(convertToList(sd.getReferenceNumbersList(), ReferenceNumbersRequestV2.class));
        cs.setPackings(convertToList(sd.getPackingList(), PackingRequestV2.class));
        cs.setDocs(convertToList(sd.getFileRepoList(), FileRepoRequestV2.class));
        cs.setELDetails(convertToList(sd.getElDetailsList(), ElDetailsRequestV2.class));
        cs.setBookingCarriages(convertToList(sd.getBookingCarriagesList(), BookingCarriageRequestV2.class));


        return null;
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

    private void mapCarrierDetails(CustomShipmentRequest cs, ShipmentDetails sd) {

    }

    private void mapAdditionalDetails(CustomShipmentRequest cs, ShipmentDetails sd) {

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
