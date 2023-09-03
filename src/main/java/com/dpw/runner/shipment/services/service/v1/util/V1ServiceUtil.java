package com.dpw.runner.shipment.services.service.v1.util;

import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.entity.*;

import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

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
                .Carrier(customerBooking.getCarrierDetails() != null ? customerBooking.getCarrierDetails().getShippingLine() : null)
                .VesselName(customerBooking.getCarrierDetails() != null ? customerBooking.getCarrierDetails().getVessel() : null)
                .Packs(Long.valueOf(customerBooking.getQuantity()))
                .PacksUnit(customerBooking.getQuantityUnit())
                .Weight(customerBooking.getGrossWeight())
                .WeightUnit(customerBooking.getGrossWeightUnit())
                .VolumeWeight(customerBooking.getWeightVolume())
                .WeightVolumeUnit(customerBooking.getVolumeUnit())
                .Volume(customerBooking.getVolume())
                .VolumeUnit(customerBooking.getVolumeUnit())
                .ReferenceNo(customerBooking.getBookingNumber())
                .CreatedDate(customerBooking.getCreatedAt() != null ? DateTimeFormatter.ofPattern(CustomerBookingConstants.DATE_FORMAT).format(customerBooking.getCreatedAt()) : null)
                .ClientCode(customerBooking.getCustomer() != null ? customerBooking.getCustomer().getOrgCode() : null)
                .ClientAddressShortCode(customerBooking.getCustomer() != null ? customerBooking.getCustomer().getAddressCode() : null)
                .ConsignerCode(customerBooking.getConsignor() != null ? customerBooking.getConsignor().getOrgCode() : null)
                .ConsignerAddressCode(customerBooking.getConsignor() != null ? customerBooking.getConsignor().getAddressCode() : null)
                .ConsigneeCode(customerBooking.getConsignee() != null ? customerBooking.getConsignee().getOrgCode() : null)
                .ConsigneeAddressCode(customerBooking.getConsignee() != null ? customerBooking.getConsignee().getAddressCode() : null)
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
                .QuoteCharges(createQuoteCharges(customerBooking.getBookingCharges()))
                .build();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.QuoteCharge> createQuoteCharges(List<BookingCharges> bookingCharges) {
        if (bookingCharges == null) return null;
        return bookingCharges.stream().filter(Objects::nonNull).map(bc ->
                CreateBookingModuleInV1.BookingEntity.QuoteCharge.builder()
                        .OverseasSellCurrency(bc.getOverseasSellCurrency())
                        .ChargeTypeCode(bc.getChargeType())
                        .CostExchange(bc.getCostExchange())
                        .EstimatedCost(bc.getEstimatedCost())
                        .EstimatedRevenue(bc.getEstimatedRevenue())
                        .LocalCostAmount(bc.getLocalCostAmount())
                        .LocalCostCurrency(bc.getLocalCostCurrency())
                        .LocalSellAmount(bc.getLocalSellAmount())
                        .LocalSellCurrency(bc.getLocalSellCurrency())
                        .NoGST(bc.getNoGST())
                        .OverseasCostCurrency(bc.getOverseasCostCurrency())
                        .OverseasSellAmount(bc.getOverseasSellAmount())
                        .SellExchange(bc.getSellExchange())
                        .TaxPercentage(bc.getTaxPercentage())
                        .ContainersGuid(createContainersGuid(bc))
                        .build()).collect(Collectors.toList());
    }

    private static List<UUID> createContainersGuid(BookingCharges bc) {
        if (bc.getContainersList() == null)
            return new ArrayList<>();
        return bc.getContainersList().stream().filter(Objects::nonNull)
                .map(container -> container.getGuid()).collect(Collectors.toList());
    }

    private static List<CreateBookingModuleInV1.BookingEntity.OrgDetail> createOrgDetails(CustomerBooking customerBooking) {
        if (customerBooking == null)
            return null;
        List<CreateBookingModuleInV1.BookingEntity.OrgDetail> list = new ArrayList<>();
        var consignee = convertParty(customerBooking.getConsignee());
        var consignor = convertParty(customerBooking.getConsignor());
        var notify = convertParty(customerBooking.getNotifyParty());
        var customer = convertParty(customerBooking.getCustomer());
        if (consignee != null)
            list.add(consignee);
        if (consignor != null)
            list.add(consignor);
        if (notify != null)
            list.add(notify);
        if (customer != null)
            list.add(customer);
        return list;
    }

    private static CreateBookingModuleInV1.BookingEntity.OrgDetail convertParty(Parties party) {
        if (party == null)
            return null;
        var addressData = party.getAddressData();
        var orgData = party == null || party.getOrgData() == null ? Collections.emptyMap() : party.getOrgData();
        return CreateBookingModuleInV1.BookingEntity.OrgDetail.builder()
                .OrgSource(PartiesConstants.API)
                .OrganizationCode(party != null ? party.getOrgCode() : null)
                .FullName((String) orgData.get(PartiesConstants.FULLNAME))
                .Address1((String) orgData.get(PartiesConstants.ADDRESS1))
                .Address2((String) orgData.get(PartiesConstants.ADDRESS2))
                .Addresses(List.of(CreateBookingModuleInV1.BookingEntity.OrgDetail.OrgDetailAddress.builder()
                        .CompanyName((String) orgData.get(PartiesConstants.FULLNAME))
                        .AddressShortCode((String) addressData.get("AddressShortCode"))
                        .Address1((String) orgData.get(PartiesConstants.ADDRESS1))
                        .SiteIdentifier((String) addressData.get(PartiesConstants.SITE_IDENTIFIER))
                        .Country(addressData.containsKey("Country") ? (String) addressData.get("Country") :
                                (String) orgData.get(PartiesConstants.COUNTRY)).build()))
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
        if (packingList == null)
            return null;
        return packingList.stream().filter(Objects::nonNull)
                .map(packing -> CreateBookingModuleInV1.BookingEntity.LooseCargo.builder()
                        .ReferenceGuid(packing.getGuid())
                        .Packs(packing.getPacks() == null ? 0 : Long.valueOf(packing.getPacks()))
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
                        .build()).collect(Collectors.toList());
    }

    private static List<CreateBookingModuleInV1.BookingEntity.Document> createDocuments(List<FileRepo> fileRepoList) {
        if (fileRepoList == null)
            return null;
        return fileRepoList.stream().filter(Objects::nonNull).map(fileRepo -> CreateBookingModuleInV1.BookingEntity.Document.builder()
                .ClientEnabled(fileRepo.getClientEnabled())
                .DocType(fileRepo.getDocType())
                .Path(fileRepo.getPath())
                .FileName(fileRepo.getFileName())
                .EventCode(fileRepo.getEventCode())
                .build()).collect(Collectors.toList());
    }

    private static List<CreateBookingModuleInV1.BookingEntity.Routing> createRoutingList(List<Routings> routingList) {
        if (routingList == null)
            return null;
        return routingList.stream().filter(Objects::nonNull).map(routings ->
                CreateBookingModuleInV1.BookingEntity.Routing.builder()
                        .ReferenceGuid(routings.getGuid())
                        .Leg(routings.getLeg())
                        .Mode(routings.getMode())
                        .PolCode(routings.getPol())
                        .PodCode(routings.getPod())
                        .build()
        ).collect(Collectors.toList());
    }

    private static List<CreateBookingModuleInV1.BookingEntity.QuoteContainer> createContainers(List<Containers> containersList) {
        if (containersList == null)
            return null;
        return containersList.stream().filter(Objects::nonNull).map(container ->
                CreateBookingModuleInV1.BookingEntity.QuoteContainer.builder()
                        .ContainerTypeCode(container.getContainerCode())
                        .Count(container.getContainerCount())
                        .CommodityCode(container.getCommodityCode())
                        .Weight(container.getGrossWeight())
                        .WeightUnit(container.getGrossWeightUnit())
                        .ReferenceGuid(container.getGuid())
                        .build()
        ).collect(Collectors.toList());
    }

}
