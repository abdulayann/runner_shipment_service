package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.nimbusds.jose.util.Pair;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Component
public class SyncEntityConversionService {

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    public List<PackingRequestV2> packingsV2ToV1(List<Packing> packingList, List<Containers> containers, UUID shipmentGuid, UUID consoleGuid) {
        Map<Long, String> map = new HashMap<>();
        if(containers != null)
            map = containers.stream().filter(container -> container.getContainerNumber() != null).collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        if(packingList != null) {
            Map<Long, String> finalMap = map;
            List<PackingRequestV2> res = packingList.stream()
                    .map(item -> {
                        PackingRequestV2 p;
                        p = modelMapper.map(item, PackingRequestV2.class);
                        p.setOriginName(item.getOrigin());
                        p.setOrigin(null);
                        p.setCommodityCode(item.getCommodity());
                        if(item.getContainerId() != null && finalMap.containsKey(item.getContainerId()))
                            p.setContainerNumber(finalMap.get(item.getContainerId()));

                        if(shipmentGuid == null && item.getShipmentId() != null) {
                            try { p.setShipmentGuid(shipmentDao.findById(p.getShipmentId()).get().getGuid()); }  catch (Exception ignored) { }
                        }
                        else p.setShipmentGuid(shipmentGuid);

                        if(consoleGuid == null && item.getConsolidationId() != null) {
                            try { p.setConsolidationGuid(consolidationDetailsDao.findById(p.getConsolidationId()).get().getGuid()); }  catch (Exception ignored) { }
                        }
                        else p.setConsolidationGuid(consoleGuid);

                        return p;
                    })
                    .collect(Collectors.toList());
            return res;
        }
        return new ArrayList<>();
    }

    public List<Packing> packingsV1ToV2(List<PackingRequestV2> packingRequestV2List) {
        if(packingRequestV2List != null) {
            List<Packing> res = packingRequestV2List.stream().map(
                    this::packingV1ToV2
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public Packing packingV1ToV2(PackingRequestV2 packingRequestV2) {
        var packing = modelMapper.map(packingRequestV2, Packing.class);
        packing.setOrigin(packingRequestV2.getOriginName());
        packing.setCommodity(packingRequestV2.getCommodityCode());
        if(packingRequestV2.getConsolidationGuid() != null) {
            try {
                packing.setConsolidationId(consolidationDetailsDao.findByGuid(packingRequestV2.getConsolidationGuid()).get().getId());
            } catch (Exception ignored) {
                packing.setConsolidationId(null);
            }
        }
        if(packingRequestV2.getShipmentGuid() != null) {
            try {
                packing.setShipmentId(shipmentDao.findByGuid(packingRequestV2.getShipmentGuid()).get().getId());
            } catch (Exception ignored) {
                packing.setShipmentId(null);
            }
        }

        return packing;
    }

    public List<ContainerRequestV2> containersV2ToV1(List<Containers> containersList) {
        if(containersList != null) {
            List<ContainerRequestV2> res = containersList.stream().map(
                    this::containerV2ToV1
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public ContainerRequestV2 containerV2ToV1(Containers containers) {
        var containerRequestV2 = modelMapper.map(containers, ContainerRequestV2.class);
        containerRequestV2.setIsHazardous(containers.getHazardous());
        containerRequestV2.setDgClassString(containers.getDgClass());
        containerRequestV2.setMarksnNums(containers.getMarksNums());
        containerRequestV2.setContainerStuffingLocationName(containers.getContainerStuffingLocation());
        return containerRequestV2;
    }

    public List<Containers> containersV1ToV2(List<ContainerRequestV2> containersList) {
        if(containersList != null) {
            List<Containers> res = containersList.stream().map(
                    this::containerV1ToV2
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public Containers containerV1ToV2(ContainerRequestV2 containerRequestV2) {
        var containers = modelMapper.map(containerRequestV2, Containers.class);
        containers.setHazardous(containerRequestV2.getIsHazardous());
        containers.setDgClass(containerRequestV2.getDgClassString());
        containers.setMarksNums(containerRequestV2.getMarksnNums());
        containers.setContainerStuffingLocation(containerRequestV2.getContainerStuffingLocationName());
        if(containerRequestV2.getConsolidationGuid() != null) {
            try {
                containers.setConsolidationId(consolidationDetailsDao.findByGuid(containerRequestV2.getConsolidationGuid()).get().getId());
            } catch (Exception ignored) {
                containers.setConsolidationId(null);
            }
        }
        if(containerRequestV2.getShipmentGuids() != null && containerRequestV2.getShipmentGuids().size() > 0) {
            try {
                ListCommonRequest listCommonRequest = constructListCommonRequest("guid", containerRequestV2.getShipmentGuids(), "IN");
                Pair<Specification<ShipmentDetails>, org.springframework.data.domain.Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
                Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
                if(shipmentDetailsPage != null && !shipmentDetailsPage.isEmpty()) {
                    List<ShipmentDetails> shipmentDetails = new ArrayList<>();
                    for (ShipmentDetails shipmentDetails1 : shipmentDetailsPage.getContent()) {
                        ShipmentDetails sd = new ShipmentDetails();
                        sd.setId(shipmentDetails1.getId());
                        shipmentDetails.add(sd);
                    }
                    containers.setShipmentsList(shipmentDetails);
                }
            } catch (Exception ignored) {}
        }
        return containers;
    }

    public List<RoutingsRequestV2> routingsV2ToV1(List<Routings> routingsList) {
        if(routingsList != null) {
            List<RoutingsRequestV2> res = routingsList.stream().map(
                    this::routingV2ToV1
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public RoutingsRequestV2 routingV2ToV1(Routings routings) {
        var routingsRequestV2 = modelMapper.map(routings, RoutingsRequestV2.class);
        if(routingsRequestV2.getMode() != null && routingsRequestV2.getMode().equals(Constants.TRANSPORT_MODE_ROA))
            routingsRequestV2.setVoyage(routings.getTruckReferenceNumber());
        routingsRequestV2.setIsDomestic(routings.isDomestic());
        routingsRequestV2.setByCarrier(routings.getCarrier());
        return routingsRequestV2;
    }

    public List<Routings> routingsV1ToV2(List<RoutingsRequestV2> routingsRequestV2List) {
        if(routingsRequestV2List != null) {
            List<Routings> res = routingsRequestV2List.stream().map(
                    this::routingV1ToV2
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public Routings routingV1ToV2(RoutingsRequestV2 routingsRequestV2) {
        var routings = modelMapper.map(routingsRequestV2, Routings.class);
        routings.setDomestic(routingsRequestV2.getIsDomestic());
        routings.setCarrier(routingsRequestV2.getByCarrier());
        if(routingsRequestV2.getMode() != null && routingsRequestV2.getMode().equals(Constants.TRANSPORT_MODE_ROA)) {
            routings.setTruckReferenceNumber(routingsRequestV2.getVoyage());
            routings.setVoyage(null);
        }
        return routings;
    }

    public List<PartyRequestV2> addressesV2ToV1(List<Parties> partiesList) {
        if(partiesList != null) {
            return partiesList.stream().map(
                    this::addressV2ToV1
            ).toList();
        }
        return new ArrayList<>();
    }

    public PartyRequestV2 addressV2ToV1(Parties parties) {
        var partyRequestV2 = modelMapper.map(parties, PartyRequestV2.class);
        partyRequestV2.setIsFreeTextAddress(parties.getIsAddressFreeText());
        if(partyRequestV2.getIsFreeTextAddress() == null)
            partyRequestV2.setIsFreeTextAddress(false);
        if(partyRequestV2.getIsFreeTextAddress()){
            var rawData = parties.getAddressData() != null ? parties.getAddressData().get("rawData"): null;
            if(rawData != null)
            partyRequestV2.setFreeTextAddress(rawData.toString());
        }
        return partyRequestV2;
    }

    public List<Parties> addressesV1ToV2(List<PartyRequestV2> partiesList) {
        if(partiesList != null) {
            return partiesList.stream().map(
                    this::addressV1ToV2
            ).toList();
        }
        return new ArrayList<>();
    }

    public Parties addressV1ToV2(PartyRequestV2 partyRequestV2) {
        var parties = modelMapper.map(partyRequestV2, Parties.class);
        parties.setIsAddressFreeText(partyRequestV2.getIsFreeTextAddress());
        return parties;
    }

    public List<EventsRequestV2> eventsV2ToV1(List<Events> eventsList) {
        if(eventsList != null) {
            List<EventsRequestV2> res = eventsList.stream().map(
                    this::eventV2ToV1
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public EventsRequestV2 eventV2ToV1(Events events) {
        EventsRequestV2 eventsRequestV2 = modelMapper.map(events, EventsRequestV2.class);
        try {
            if(Objects.equals(events.getEntityType(), Constants.SHIPMENT)) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(events.getEntityId()).get();
                eventsRequestV2.setShipmentGuid(shipmentDetails.getGuid());
            }
            if(Objects.equals(events.getEntityType(), Constants.CONSOLIDATION)) {
                ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(events.getEntityId()).get();
                eventsRequestV2.setConsolidationGuid(consolidationDetails.getGuid());
            }
        } catch (Exception ignored) {}
        return eventsRequestV2;
    }

    public MawbStocksV2 mawbStocksV2ToV1(MawbStocks mawbStocks) {
        MawbStocksV2 response = modelMapper.map(mawbStocks, MawbStocksV2.class);
        if(mawbStocks.getConsolidationId() != null) {
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(mawbStocks.getConsolidationId()).get();
            response.setConsolidationGuid(consolidationDetails.getGuid());
        }

        for(var mawbStocksLink : response.getMawbStocksLinkRows()) {
            if(mawbStocksLink.getEntityType().equalsIgnoreCase("SHIPMENT")) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(mawbStocksLink.getEntityId()).get();
                mawbStocksLink.setShipmentGuid(shipmentDetails.getGuid());
            }
            else if(mawbStocksLink.getEntityType().equalsIgnoreCase("CONSOLIDATION")) {
                ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(mawbStocksLink.getEntityId()).get();
                mawbStocksLink.setConsolidationGuid(consolidationDetails.getGuid());
            }
        }

        return response;
    }

    public MawbStocks mawbStocksV1ToV2(MawbStocksV2 mawbStocks) {
        MawbStocks response = modelMapper.map(mawbStocks, MawbStocks.class);
        if(mawbStocks.getConsolidationGuid() != null) {
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findByGuid(mawbStocks.getConsolidationGuid()).get();
            response.setConsolidationId(consolidationDetails.getId());
        }

        if(mawbStocks.getMawbStocksLinkRows() != null && mawbStocks.getMawbStocksLinkRows().size() > 0) {
            List<MawbStocksLink> mawbStocksLinks = new ArrayList<>();
            for(var mawbStocksLinkV2 : mawbStocks.getMawbStocksLinkRows()) {
                if(mawbStocksLinkV2.getEntityType().equalsIgnoreCase("SHIPMENT")) {
                    Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(mawbStocksLinkV2.getShipmentGuid());
                    shipmentDetails.ifPresent(details -> mawbStocksLinkV2.setEntityId(details.getId()));
                }
                else if(mawbStocksLinkV2.getEntityType().equalsIgnoreCase("CONSOLIDATION")) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(mawbStocksLinkV2.getConsolidationGuid());
                    consolidationDetails.ifPresent(details -> mawbStocksLinkV2.setEntityId(details.getId()));
                }
                mawbStocksLinks.add(modelMapper.map(mawbStocksLinkV2, MawbStocksLink.class));
            }
            response.setMawbStocksLinkRows(mawbStocksLinks);
        }

        return response;
    }

}
