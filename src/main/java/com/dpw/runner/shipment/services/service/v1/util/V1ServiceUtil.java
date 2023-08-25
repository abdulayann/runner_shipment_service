package com.dpw.runner.shipment.services.service.v1.util;

import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.entity.*;

import java.time.format.DateTimeFormatter;
import java.util.*;

public class V1ServiceUtil {

    public static CreateBookingModuleInV1 createBookingRequestForV1(CustomerBooking customerBooking) {
        return CreateBookingModuleInV1.builder()
                .IsP100Booking(Boolean.TRUE)
                .Entity(createEntity(customerBooking))
                .build();
    }

    private static CreateBookingModuleInV1.BookingEntity createEntity(CustomerBooking customerBooking) {
        var carrierDetails = Optional.ofNullable(customerBooking.getCarrierDetails());
        return CreateBookingModuleInV1.BookingEntity.builder()
                .ContractId(customerBooking.getContractId())
                .NotifyPartyCode(Objects.isNull(customerBooking.getNotifyParty()) ? null : customerBooking.getNotifyParty().getOrgCode())
                .NotifyPartyAddressCode(Objects.isNull(customerBooking.getNotifyParty()) ? null : customerBooking.getNotifyParty().getAddressCode())
                .Carrier(customerBooking.getCarrierDetails().getShippingLine())
                .VesselName(customerBooking.getCarrierDetails().getVessel())
                .Packs(Long.valueOf(customerBooking.getQuantity()))
                .PacksUnit(customerBooking.getQuantityUnit())
                .Weight(customerBooking.getGrossWeight())
                .WeightUnit(customerBooking.getGrossWeightUnit())
                .VolumeWeight(customerBooking.getWeightVolume())
                .WeightVolumeUnit(customerBooking.getVolumeUnit())
                .Volume(customerBooking.getVolume())
                .VolumeUnit(customerBooking.getVolumeUnit())
                .ReferenceNo(customerBooking.getBookingNumber())
                .CreatedDate(DateTimeFormatter.ofPattern(CustomerBookingConstants.DATE_FORMAT).format(customerBooking.getCreatedAt()))
                .ClientCode(customerBooking.getCustomer().getOrgCode())
                .ClientAddressShortCode(customerBooking.getCustomer().getAddressCode())
                .ConsignerCode(customerBooking.getConsignor().getOrgCode())
                .ConsignerAddressCode(customerBooking.getConsignor().getAddressCode())
                .ConsigneeCode(customerBooking.getConsignee().getOrgCode())
                .ConsigneeAddressCode(customerBooking.getConsignee().getAddressCode())
                .OriginCode(carrierDetails.map(c -> c.getOrigin()).orElse(null))
                .DestinationCode(carrierDetails.map(c -> c.getDestination()).orElse(null))
                .originPortCode(carrierDetails.map(c -> c.getOriginPort()).orElse(null))
                .DestinationPortCode(carrierDetails.map(c -> c.getDestinationPort()).orElse(null))
                .ConsolidationType(CustomerBookingConstants.STD)
                .TransportMode(customerBooking.getTransportType())
                .ContainerType(customerBooking.getCargoType())
                .CustomShipmentType(customerBooking.getDirection())
                .Incoterm(customerBooking.getIncoTerms())
                .ServiceMode(customerBooking.getServiceMode())
                .IsShipmentCreateEnabled(Boolean.TRUE)
                .IsConsolidationCreateEnabled(customerBooking.getCargoType().equals(CustomerBookingConstants.FCL))
                .BookingType(CustomerBookingConstants.ONLINE)
                .Status(CustomerBookingConstants.ONE)
                .QuoteContainers(createContainers(customerBooking.getContainersList()))
                .RoutingList(createRoutingList(customerBooking.getRoutingList()))
                .Documents(createDocuments(customerBooking.getFileRepoList()))
                .Loosecargos(createLooseCarges(customerBooking.getPackingList()))
                .OrgDetails(createOrgDetails(customerBooking))
                .build();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.OrgDetail> createOrgDetails(CustomerBooking customerBooking) {
        List<CreateBookingModuleInV1.BookingEntity.OrgDetail> list = new ArrayList<>();
        list.add(convertParty(customerBooking.getConsignee()));
        list.add(convertParty(customerBooking.getConsignor()));
        list.add(convertParty(customerBooking.getNotifyParty()));
        list.add(convertParty(customerBooking.getCustomer()));
        return list;
    }

    private static CreateBookingModuleInV1.BookingEntity.OrgDetail convertParty(Parties party) {
        var orgData = party == null || party.getOrgData() == null ? Collections.emptyMap() : party.getOrgData();
        return CreateBookingModuleInV1.BookingEntity.OrgDetail.builder()
                .OrgSource(PartiesConstants.API)
                .OrganizationCode(party != null ? party.getOrgCode() : null)
                .FullName((String) orgData.get(PartiesConstants.FULLNAME))
                .Address1((String) orgData.get(PartiesConstants.ADDRESS1))
                .Address2((String) orgData.get(PartiesConstants.ADDRESS2))
                .Country((String) orgData.get(PartiesConstants.COUNTRY))
                .CityCode((String) orgData.get(PartiesConstants.CITY_CODE))
                .State((String) orgData.get(PartiesConstants.STATE))
                .ZipPostCode((String) orgData.get(PartiesConstants.ZIP_POST_CODE))
                .UnlocoCode((String) orgData.get(PartiesConstants.UNLOCO_CODE))
                .CurrencyCode((String) orgData.get(PartiesConstants.CURRENCY_CODE))
                .Phone((String) orgData.get(PartiesConstants.PHONE))
                .Mobile((String) orgData.get(PartiesConstants.MOBILE))
                .Fax((String) orgData.get(PartiesConstants.FAX))
                .Email((String) orgData.get(PartiesConstants.EMAIL))
                .ActiveClient(Boolean.TRUE)
                .DefaultAddressSiteIdentifier(PartiesConstants.SITE)
                .Receivables(Boolean.TRUE)
                .build();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.LooseCargo> createLooseCarges(List<Packing> packingList) {
        return packingList.stream().filter(Objects::nonNull)
                .map(packing -> CreateBookingModuleInV1.BookingEntity.LooseCargo.builder()
                        .ReferenceGuid(packing.getGuid())
                        .Packs(Long.valueOf(packing.getPacks()))
                        .PacksUnit(packing.getPacksType())
                        .Length(packing.getLength())
                        .Height(packing.getHeight())
                        .Width(packing.getWidth())
                        .Du(packing.getLengthUnit())
                        .Weight(packing.getWeight())
                        .WeightUnit(packing.getWeightUnit())
                        .Volume(packing.getVolume())
                        .VolumeUnit(packing.getVolumeUnit())
                        .Chargeable(packing.getChargeable())
                        .ChargeableUnit(packing.getChargeableUnit())
                        .GoodsDescription(packing.getGoodsDescription())
                        .CommodityCode(packing.getCommodity())
                        .HazardousCheckBox(packing.getHazardous())
                        .HsCode(packing.getHSCode())
                        .build()).toList();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.Document> createDocuments(List<FileRepo> fileRepoList) {
        return fileRepoList.stream().filter(Objects::nonNull).map(fileRepo -> CreateBookingModuleInV1.BookingEntity.Document.builder()
                .ClientEnabled(fileRepo.getClientEnabled())
                .DocType(fileRepo.getDocType())
                .Path(fileRepo.getPath())
                .FileName(fileRepo.getFileName())
                .EventCode(fileRepo.getEventCode())
                .build()).toList();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.Routing> createRoutingList(List<Routings> routingList) {
        return routingList.stream().filter(Objects::nonNull).map(routings ->
                CreateBookingModuleInV1.BookingEntity.Routing.builder()
                        .ReferenceGuid(routings.getGuid())
                        .Leg(routings.getLeg())
                        .Mode(routings.getMode())
                        .PolCode(routings.getPol())
                        .PodCode(routings.getPod())
                        .build()
        ).toList();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.QuoteContainer> createContainers(List<Containers> containersList) {
        return containersList.stream().filter(Objects::nonNull).map(container ->
                CreateBookingModuleInV1.BookingEntity.QuoteContainer.builder()
                        .ContainerTypeCode(container.getContainerCode())
                        .Count(container.getContainerCount())
                        .CommodityCode(container.getCommodityCode())
                        .Weight(container.getGrossWeight())
                        .WeightUnit(container.getGrossWeightUnit())
                        .ReferenceGuid(container.getGuid())
                        .build()
        ).toList();
    }

}
