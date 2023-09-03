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

        // Fully auto-mapped entities
        // Events, jobs, referenceNumbers, docs, elDetails, services, notes
        // packing (except OriginName field)

        //Mapping root party objects that are not auto-mapped
        cs.setConsignerParty(mapPartyObject(sd.getConsigner()));
        cs.setConsigneeParty(mapPartyObject(sd.getConsignee()));

        // assigning child entities not automatically mapped
        // entityID also gets assigned as a part of this mapping
        mapTruckDriverDetail(cs, sd);
        cs.setRoutings(convertToList(sd.getRoutingsList(), RoutingsRequestV2.class));
        cs.setReferenceNumbers(convertToList(sd.getReferenceNumbersList(), ReferenceNumbersRequestV2.class));
        cs.setPackings_(convertToList(sd.getPackingList(), PackingRequestV2.class));
        cs.setDocs_(convertToList(sd.getFileRepoList(), FileRepoRequestV2.class));
        cs.setELDetails(convertToList(sd.getElDetailsList(), ElDetailsRequestV2.class));

        // Container missing mappings
        // dgClassString, IsHazardous, MarksnNums
        // PickupAddressJSON and DeliveryAddressJSON (could be renamed for easy mapping)

        cs.setBookingCarriages(convertToList(sd.getBookingCarriagesList(), BookingCarriageRequestV2.class));

        return cs;
    }
    
    public ShipmentDetails reverseSync(CustomShipmentRequest cs) {
        ShipmentDetails sd = new ShipmentDetails();
        mapCarrierDetailsReverse(sd, cs);
        mapAdditionalDetailsReverse(sd, cs);

        sd = modelMapper.map(cs, ShipmentDetails.class);

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
        sd.setPackingList(convertToList(cs.getPackings_(), Packing.class));
        sd.setFileRepoList(convertToList(cs.getDocs_(), FileRepo.class));
        sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

        sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));

        return sd;
    }

    private PartyRequestV2 mapPartyObject(Parties sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, PartyRequestV2.class);
    }

    private Parties mapPartyObject(PartyRequestV2 sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, Parties.class);
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
        cs.setDestinationName(sd.getCarrierDetails().getDestination());
        cs.setDestinationPortName(sd.getCarrierDetails().getDestinationPort());
        cs.setOriginName(sd.getCarrierDetails().getOrigin());
        cs.setOriginPortName(sd.getCarrierDetails().getOriginPort());

    }

    private void mapCarrierDetailsReverse(ShipmentDetails sd, CustomShipmentRequest cs) {
        if(sd.getCarrierDetails() == null)
            return;
        CarrierDetails carrierDetails = modelMapper.map(cs, CarrierDetails.class);
        carrierDetails.setDestination(cs.getDestinationName());
        carrierDetails.setDestinationPort(cs.getDestinationPortName());
        carrierDetails.setOrigin(cs.getOriginName());
        carrierDetails.setOriginPort(cs.getOriginPortName());
        sd.setCarrierDetails(carrierDetails);
    }

    private void mapAdditionalDetails(CustomShipmentRequest cs, ShipmentDetails sd) {
        if(sd.getAdditionalDetails() == null)
            return;
        modelMapper.map(sd.getAdditionalDetails(), cs);
        cs.setReceivingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getReceivingForwarder()));
        cs.setSendingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getSendingForwarder()));
        cs.setTraderOrSupplierParty(mapPartyObject(sd.getAdditionalDetails().getTraderOrSupplier()));
    }

    private void mapAdditionalDetailsReverse(ShipmentDetails sd, CustomShipmentRequest cs) {
        if(sd.getAdditionalDetails() == null)
            return;
        AdditionalDetails additionalDetails = modelMapper.map(cs, AdditionalDetails.class);
        additionalDetails.setReceivingForwarder(mapPartyObject(cs.getReceivingForwarderParty()));
        additionalDetails.setSendingForwarder(mapPartyObject(cs.getSendingForwarderParty()));
        additionalDetails.setTraderOrSupplier(mapPartyObject(cs.getTraderOrSupplierParty()));
        sd.setAdditionalDetails(additionalDetails);
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
