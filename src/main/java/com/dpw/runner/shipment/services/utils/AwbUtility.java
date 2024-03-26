package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dto.request.awb.AwbAddressParam;
import com.dpw.runner.shipment.services.dto.request.awb.AwbPackingInfo;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ChargeBasis;
import com.dpw.runner.shipment.services.entity.enums.ChargesDue;
import com.dpw.runner.shipment.services.entity.enums.RateClass;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("rawtypes")
@Component
public class AwbUtility {
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    public MasterDataUtils masterDataUtils;
    @Autowired
    private ModelMapper modelMapper;

    private AwbUtility(){}
    public static String getFormattedAddress(AwbAddressParam addressParam)
    {
        String forMattedAddress = "";
        String newLine = "\r\n";
        forMattedAddress = addressParam.getAddress1();
        if (!StringUtility.isEmpty(addressParam.getAddress2()))
            forMattedAddress += newLine + addressParam.getAddress2();
        if (!StringUtility.isEmpty(addressParam.getState()))
            forMattedAddress += newLine + addressParam.getState();
        if (!StringUtility.isEmpty(addressParam.getCity()))
            forMattedAddress += newLine + addressParam.getCity();
        if (!StringUtility.isEmpty(addressParam.getCountry()))
            forMattedAddress += newLine + addressParam.getCountry();
        if (!StringUtility.isEmpty(addressParam.getPinCode()))
            forMattedAddress += newLine + addressParam.getPinCode();
        if (!StringUtility.isEmpty(addressParam.getContactNumber()))
            forMattedAddress += newLine + addressParam.getContactNumber();
        return forMattedAddress;
    }

    public static String constructAddress(Map<String, Object> addressData) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";

        if(addressData != null) {
            if (addressData.containsKey(PartiesConstants.ADDRESS1))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS1)));
            if (addressData.containsKey(PartiesConstants.ADDRESS2))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS2)));
            if (addressData.containsKey(PartiesConstants.STATE))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.STATE)));
            if (addressData.containsKey(PartiesConstants.CITY))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CITY)));
            if (addressData.containsKey(PartiesConstants.COUNTRY))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.COUNTRY)));
            if (addressData.containsKey(PartiesConstants.ZIP_POST_CODE))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ZIP_POST_CODE)));
            if (addressData.containsKey(PartiesConstants.CONTACT_PHONE))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CONTACT_PHONE)));
        }

        return sb.toString();
    }

    public static String generateNatureAndQuantGoodsField(String goodsDescription, BigDecimal volumeWeight, List<AwbPackingInfo> packingList) {
        String natureAndQuantGoodsValue = goodsDescription != null ? goodsDescription : "";
        String packsDescriptionValue = "";
        String dimensionText = "DIMS: In ";
        HashSet<String> uniqueDimension = new HashSet<String>();
        String newLine = "\r\n";
        char[] toTrim = {',', ' '};
        String volumetricWeight = String.format("{0:0.0#}", volumeWeight);
        String dimnAndPacksText = "";

        if (!StringUtility.isEmpty(natureAndQuantGoodsValue)) {
            natureAndQuantGoodsValue += newLine;
        }
        if (packingList != null && packingList.size() > 0) {
            var counter = 0;
            for (var packing : packingList) {
                String pieces = " ";
                String len = " ";
                String width = " ";
                String height = " ";
                String equals = "=";
                String cross = "X";

                if (!StringUtility.isEmpty(packing.getPacks())) {
                    pieces = packing.getPacks() + equals;
                } else {
                    pieces += equals;
                }

                if (packing.getLength() != null) {
                    len = packing.getLength() + cross;
                } else {
                    len += cross;
                }

                if (packing.getWidth() != null) {
                    width = packing.getWidth() + cross;
                } else {
                    width += cross;
                }

                if (packing.getHeight() != null) {
                    height = packing.getHeight().toString();
                }
                if (!StringUtility.isEmpty(packing.getLengthUnit()) &&
                        !StringUtility.isEmpty(packing.getWidthUnit()) &&
                        !StringUtility.isEmpty(packing.getHeightUnit())) {
                    uniqueDimension.add(packing.getLengthUnit());
                    uniqueDimension.add(packing.getWidthUnit());
                    uniqueDimension.add(packing.getHeightUnit());
                }
                counter++;

                packsDescriptionValue += pieces + len + width + height + ",";
                if (counter == packingList.size()) {
                    packsDescriptionValue = StringUtils.stripEnd(packsDescriptionValue, ",");
                    packsDescriptionValue = StringUtils.stripEnd(packsDescriptionValue, " ");
                }

                if (counter % 2 == 0) {
                    packsDescriptionValue += newLine;
                }
            }


            if (uniqueDimension.size() == 1) {
                String dimentionUnit = uniqueDimension.stream().findFirst().get();
                if (dimentionUnit == "CM") {
                    dimentionUnit = "CMS";
                } else if (dimentionUnit == "IN") {
                    dimentionUnit = "Inches";
                } else if (dimentionUnit == "M") {
                    dimentionUnit = "Meter";
                } else if (dimentionUnit == "FT") {
                    dimentionUnit = "Feet";
                } else {
                    dimentionUnit = "";
                }

                dimensionText += dimentionUnit + newLine;
            } else {
                dimensionText += newLine;
            }

            if (counter % 2 != 0) {
                packsDescriptionValue += newLine;
            }
            packsDescriptionValue += "Total Volumetric Weight ";

//            if (tenantSettings != null && tenantSettings.WeightChargeableUnit == "KG") { //TODO fetch values from tenant
//                packsDescriptionValue += volumetricWeight + " " + "KGS";
//            }

            dimnAndPacksText = dimensionText + packsDescriptionValue;

        }
        return natureAndQuantGoodsValue + dimnAndPacksText;
    }

    public static BigDecimal roundOffAirShipment(double charge) {
        if ((charge - 0.50) <= Math.floor(charge) && charge != Math.floor(charge))
            charge = Math.floor(charge) + 0.50;
        else
            charge = Math.ceil(charge);
        return new BigDecimal(charge);
    }

    public static void validateShipmentInfoBeforeGeneratingAwb(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getConsigner() == null || shipmentDetails.getConsigner().getOrgCode() == null) {
            throw new ValidationException("Consigner details are required in shipment to generate the document.");
        }
        if (shipmentDetails.getConsignee() == null || shipmentDetails.getConsignee().getOrgCode() == null) {
            throw new ValidationException("Consignee details are required in shipment to generate the document.");
        }
        if (shipmentDetails.getCarrierDetails() == null || shipmentDetails.getCarrierDetails().getShippingLine() == null) {
            throw new ValidationException("Flight carrier details are required in shipment to generate the document.");
        }
        if (shipmentDetails.getCarrierDetails().getOriginPort() == null) {
            throw new ValidationException("Port Of Loading is required in shipment to generate the document.");
        }
        if ( shipmentDetails.getCarrierDetails().getDestinationPort() == null) {
            throw new ValidationException("Port Of Destination is required in shipment to generate the document.");
        }
        if (shipmentDetails.getCarrierDetails().getId() == null) {
            throw new ValidationException("Carrier is required in shipment to generate the document.");
        }
        if (Objects.equals(shipmentDetails.getShipmentType(), ShipmentConstants.SHIPMENT_TYPE_DRT) && (shipmentDetails.getMasterBill() == null || shipmentDetails.getMasterBill().isBlank())) {
            throw new ValidationException("MAWB Number is required in shipment to generate the document.");
        }
        if (shipmentDetails.getHouseBill() == null || shipmentDetails.getHouseBill().isBlank()) {
            throw new ValidationException("HAWB Number is required in shipment to generate the document.");
        }
    }

    public static void validateConsolidationInfoBeforeGeneratingAwb(ConsolidationDetails consolidationDetails)
    {
        if (consolidationDetails.getSendingAgent() == null || consolidationDetails.getSendingAgent().getOrgCode() == null)
        {
            throw new ValidationException("Sending Agent details are required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getReceivingAgent() == null || consolidationDetails.getReceivingAgent().getOrgCode() == null)
        {
            throw new ValidationException("Receiving Agent details are required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getCarrierDetails() == null)
        {
            throw new ValidationException("Carrier is required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getCarrierDetails().getOriginPort() == null)
        {
            throw new ValidationException("Loading Port is required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getCarrierDetails().getDestinationPort() == null)
        {
            throw new ValidationException("Discharge Port is required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getMawb() == null || consolidationDetails.getMawb().isBlank()) {
            throw new ValidationException("MAWB number can't be null for generating MAWB !");
        }
    }

    public static String generateNatureAndQuantFieldsForConsolMawb(String goodsDescription, BigDecimal volumeWeight, List<AwbPackingInfo> packingList)
    {
        String defaultTextForQuantAndGoods = Constants.DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB;
        String newLine = "\r\n";
        return defaultTextForQuantAndGoods + newLine + generateNatureAndQuantGoodsField(goodsDescription, volumeWeight, packingList);
    }

    public AwbAirMessagingResponse createAirMessagingRequestForConsole(Awb awb, ConsolidationDetails consolidationDetails) {
        TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        AwbAirMessagingResponse awbResponse = jsonHelper.convertValue(awb, AwbAirMessagingResponse.class);
        awbResponse.setMeta(AwbAirMessagingResponse.Meta.builder().build());
        this.populateEnums(awbResponse);

        List<Parties> orgLists = new ArrayList<>();
        Parties issuingAgent = null;
        for (var orgRow : consolidationDetails.getConsolidationAddresses()) {
            if (orgRow.getType().equals(Constants.FORWARDING_AGENT)) {
                issuingAgent = orgRow;
            }
        }
        if(consolidationDetails.getSendingAgent() != null){
            orgLists.add(consolidationDetails.getSendingAgent());
        }
        if(consolidationDetails.getReceivingAgent() != null){
            orgLists.add(consolidationDetails.getReceivingAgent());
        }
        if(issuingAgent != null) {
            orgLists.add(issuingAgent);
        }
        OrgAddressResponse response = v1ServiceUtil.fetchOrgInfoFromV1(orgLists);
        if(issuingAgent == null){
            var orgs = masterDataUtils.fetchOrganizations("Id", tenantModel.getDefaultOrgId());
            if(!orgs.isEmpty()) {
                awbResponse.getMeta().setIssueingAgent(AwbAirMessagingResponse.OrgDetails.builder()
                        .city(orgs.get(0).getCity())
                        .country(StringUtility.isNotEmpty(orgs.get(0).getCountry()) ? CountryListHelper.ISO3166.fromAlpha3(orgs.get(0).getCountry().toUpperCase()).getAlpha2() : null)
                        .currency(orgs.get(0).getCurrencyCode())
                        .expiry(null)
                        .number(null)
                        .build());
            }
        } else {
            if(response.getOrganizations().containsKey(issuingAgent.getOrgCode())
                    && response.getAddresses().containsKey(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode())) {
                var org = response.getOrganizations().get(issuingAgent.getOrgCode());
                var address = response.getAddresses().get(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode());
                awbResponse.getMeta().setIssueingAgent(populateOrgsFields(org, address));
            }
        }

        if(consolidationDetails.getSendingAgent() != null && (response.getOrganizations().containsKey(consolidationDetails.getSendingAgent().getOrgCode())
             && response.getAddresses().containsKey(consolidationDetails.getSendingAgent().getOrgCode() + '#' + consolidationDetails.getSendingAgent().getAddressCode()))){
                var org = response.getOrganizations().get(consolidationDetails.getSendingAgent().getOrgCode());
                var address = response.getAddresses().get(consolidationDetails.getSendingAgent().getOrgCode() + '#' + consolidationDetails.getSendingAgent().getAddressCode());
                awbResponse.getMeta().setShipper(populateOrgsFields(org, address));

        }
        if(consolidationDetails.getReceivingAgent() != null && (response.getOrganizations().containsKey(consolidationDetails.getReceivingAgent().getOrgCode())
                    && response.getAddresses().containsKey(consolidationDetails.getReceivingAgent().getOrgCode() + '#' + consolidationDetails.getReceivingAgent().getAddressCode()))){
                var org = response.getOrganizations().get(consolidationDetails.getReceivingAgent().getOrgCode());
                var address = response.getAddresses().get(consolidationDetails.getReceivingAgent().getOrgCode() + '#' + consolidationDetails.getReceivingAgent().getAddressCode());
                awbResponse.getMeta().setConsignee(populateOrgsFields(org, address));

        }

        List<String> unlocoRequests = new ArrayList<>();
        List<String> carrierRequests = new ArrayList<>();

        unlocoRequests.add(consolidationDetails.getCarrierDetails().getOriginPort());
        unlocoRequests.add(consolidationDetails.getCarrierDetails().getDestinationPort());
        if(awbResponse.getAwbRoutingInfo() != null && !awbResponse.getAwbRoutingInfo().isEmpty())
            awbResponse.getMeta().setAwbRoutingInfo(jsonHelper.convertValueToList(awbResponse.getAwbRoutingInfo(), AwbAirMessagingResponse.AwbRoutingInfoRes.class));
        if(awbResponse.getMeta().getAwbRoutingInfo() != null && !awbResponse.getMeta().getAwbRoutingInfo().isEmpty()){
            for (var awbRoute: awbResponse.getMeta().getAwbRoutingInfo()){
                unlocoRequests.add(awbRoute.getOriginPortName());
                unlocoRequests.add(awbRoute.getDestinationPortName());
                carrierRequests.add(awbRoute.getByCarrier());
            }
        }

        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        Map<String, EntityTransferCarrier> carriersMap = masterDataUtils.fetchInBulkCarriers(carrierRequests);

        if(unlocationsMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            var unloc = unlocationsMap.get(consolidationDetails.getCarrierDetails().getOriginPort());
            awbResponse.getMeta().setPol(populateUnlocFields(unloc));
        }
        if(unlocationsMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            var unloc = unlocationsMap.get(consolidationDetails.getCarrierDetails().getDestinationPort());
            awbResponse.getMeta().setPod(populateUnlocFields(unloc));
        }
        if(awbResponse.getMeta().getAwbRoutingInfo() != null && !awbResponse.getMeta().getAwbRoutingInfo().isEmpty()){
            for (var awbRoute: awbResponse.getMeta().getAwbRoutingInfo()){
                if(unlocationsMap.containsKey(awbRoute.getOriginPortName())) {
                    var unloc = unlocationsMap.get(awbRoute.getOriginPortName());
                    awbRoute.setOriginIATACode(unloc.getIataCode());
                }
                if(unlocationsMap.containsKey(awbRoute.getDestinationPortName())) {
                    var unloc = unlocationsMap.get(awbRoute.getDestinationPortName());
                    awbRoute.setDestinationIATACode(unloc.getIataCode());
                }
                if(carriersMap.containsKey(awbRoute.getByCarrier())) {
                    var carrier = carriersMap.get(awbRoute.getByCarrier());
                    awbRoute.setAirlineInfo(AwbAirMessagingResponse.AirlineInfo.builder()
                            .airlinePrefix(carrier.getAirlineCode())
                            .iata(carrier.getIATACode())
                            .build());
                }
            }
        }
        if(awbResponse.getAwbPaymentInfo() != null)
            awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalCollect().max(awbResponse.getAwbPaymentInfo().getTotalPrepaid()));
        awbResponse.getMeta().setTenantInfo(populateTenantInfoFields(tenantModel, shipmentSettingsDetails));
        return awbResponse;
    }
    private void populateEnums(AwbAirMessagingResponse awbResponse) {
        Map<String, String> chargeDue = Arrays.stream(ChargesDue.values()).collect(Collectors.toMap(e -> String.valueOf(e.getId()), ChargesDue::getDescription));
        Map<String, String> chargeBasis = Arrays.stream(ChargeBasis.values()).collect(Collectors.toMap(e -> String.valueOf(e.getId()), ChargeBasis::getDescription));
        Map<String, String> rateClass = Arrays.stream(RateClass.values()).collect(Collectors.toMap(e -> String.valueOf(e.getId()), RateClass::getDescription));
        awbResponse.getMeta().setChargeDue(chargeDue);
        awbResponse.getMeta().setChargeBasis(chargeBasis);
        awbResponse.getMeta().setRateClass(rateClass);
    }
    private AwbAirMessagingResponse.UnlocDetails populateUnlocFields(UnlocationsResponse unloc) {
        return AwbAirMessagingResponse.UnlocDetails.builder()
                .name(unloc.getName())
                .countyCode(StringUtility.isNotEmpty(unloc.getCountry()) ? CountryListHelper.ISO3166.fromAlpha3(unloc.getCountry().toUpperCase()).getAlpha2() : null)
                .iataCode(unloc.getIataCode())
                .locCode(unloc.getLocCode())
                .build();
    }

    private AwbAirMessagingResponse.TenantInfo populateTenantInfoFields(TenantModel tenantModel, ShipmentSettingsDetails shipmentSettingsDetails) {
        return AwbAirMessagingResponse.TenantInfo.builder()
                .pimaAddress(tenantModel.PIMAAddress)
                .number(shipmentSettingsDetails.getRaNumber())
                .expiry(shipmentSettingsDetails.getRaExpiry())
                .city(tenantModel.city)
                .country(StringUtility.isNotEmpty(tenantModel.country) ? CountryListHelper.ISO3166.fromAlpha3(tenantModel.country.toUpperCase()).getAlpha2() : null)
                .state(tenantModel.state)
                .branchCode(tenantModel.code)
                .branchName(tenantModel.tenantName)
                .build();
    }

    private AwbAirMessagingResponse.OrgDetails populateOrgsFields(Map<String, Object> org, Map<String, Object> address) {
        return AwbAirMessagingResponse.OrgDetails.builder()
                        .city(org.containsKey(PartiesConstants.CITY) ? (String) org.get(PartiesConstants.CITY) : null)
                        .country(org.containsKey(PartiesConstants.COUNTRY) && StringUtility.isNotEmpty((String)org.get(PartiesConstants.COUNTRY)) ? CountryListHelper.ISO3166.fromAlpha3(((String) org.get(PartiesConstants.COUNTRY)).toUpperCase()).getAlpha2() : null)
                        .currency(org.containsKey(PartiesConstants.CURRENCY_CODE) ? (String) org.get(PartiesConstants.CURRENCY_CODE) : null)
                        .expiry(address.containsKey(PartiesConstants.KC_RA_EXPIRY) && StringUtility.isNotEmpty((String)address.get(PartiesConstants.KC_RA_EXPIRY)) ? LocalDateTime.parse((String) address.get(PartiesConstants.KC_RA_EXPIRY)) : null)
                        .number(address.containsKey(PartiesConstants.KC_RA_NUMBER) ? (String) address.get(PartiesConstants.KC_RA_NUMBER) : null)
                .build();
    }

    public AwbAirMessagingResponse createAirMessagingRequestForShipment(Awb awb, ShipmentDetails shipmentDetails) {
        TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        AwbAirMessagingResponse awbResponse = jsonHelper.convertValue(awb, AwbAirMessagingResponse.class);
        awbResponse.setMeta(AwbAirMessagingResponse.Meta.builder().build());
        this.populateEnums(awbResponse);

        List<Parties> orgLists = new ArrayList<>();
        Parties issuingAgent = null;
        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FORWARDING_AGENT)) {
                issuingAgent = orgRow;
            }
        }
        if(shipmentDetails.getConsigner() != null){
            orgLists.add(shipmentDetails.getConsigner());
        }
        if(shipmentDetails.getConsignee() != null){
            orgLists.add(shipmentDetails.getConsignee());
        }
        if(issuingAgent != null) {
            orgLists.add(issuingAgent);
        }
        OrgAddressResponse response = v1ServiceUtil.fetchOrgInfoFromV1(orgLists);
        if(issuingAgent == null){
            var orgs = masterDataUtils.fetchOrganizations("Id", tenantModel.getDefaultOrgId());
            if(!orgs.isEmpty()) {
                awbResponse.getMeta().setIssueingAgent(AwbAirMessagingResponse.OrgDetails.builder()
                        .city(orgs.get(0).getCity())
                        .country(StringUtility.isNotEmpty(orgs.get(0).getCountry()) ? CountryListHelper.ISO3166.fromAlpha3(orgs.get(0).getCountry().toUpperCase()).getAlpha2() : null)
                        .currency(orgs.get(0).getCurrencyCode())
                        .expiry(null)
                        .number(null)
                        .build());
            }
        } else {
            if(response.getOrganizations().containsKey(issuingAgent.getOrgCode())
                    && response.getAddresses().containsKey(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode())) {
                var org = response.getOrganizations().get(issuingAgent.getOrgCode());
                var address = response.getAddresses().get(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode());
                awbResponse.getMeta().setIssueingAgent(populateOrgsFields(org, address));
            }
        }

        if(shipmentDetails.getConsigner() != null && (response.getOrganizations().containsKey(shipmentDetails.getConsigner().getOrgCode())
                    && response.getAddresses().containsKey(shipmentDetails.getConsigner().getOrgCode() + '#' + shipmentDetails.getConsigner().getAddressCode()))){
                var org = response.getOrganizations().get(shipmentDetails.getConsigner().getOrgCode());
                var address = response.getAddresses().get(shipmentDetails.getConsigner().getOrgCode() + '#' + shipmentDetails.getConsigner().getAddressCode());
                awbResponse.getMeta().setShipper(populateOrgsFields(org, address));

        }
        if(shipmentDetails.getConsignee() != null && (response.getOrganizations().containsKey(shipmentDetails.getConsignee().getOrgCode())
                    && response.getAddresses().containsKey(shipmentDetails.getConsignee().getOrgCode() + '#' + shipmentDetails.getConsignee().getAddressCode()))) {
                var org = response.getOrganizations().get(shipmentDetails.getConsignee().getOrgCode());
                var address = response.getAddresses().get(shipmentDetails.getConsignee().getOrgCode() + '#' + shipmentDetails.getConsignee().getAddressCode());
                awbResponse.getMeta().setConsignee(populateOrgsFields(org, address));

        }

        List<String> unlocoRequests = new ArrayList<>();
        List<String> carrierRequests = new ArrayList<>();

        unlocoRequests.add(shipmentDetails.getCarrierDetails().getOriginPort());
        unlocoRequests.add(shipmentDetails.getCarrierDetails().getDestinationPort());
        if(awbResponse.getAwbRoutingInfo() != null && !awbResponse.getAwbRoutingInfo().isEmpty())
            awbResponse.getMeta().setAwbRoutingInfo(jsonHelper.convertValueToList(awbResponse.getAwbRoutingInfo(), AwbAirMessagingResponse.AwbRoutingInfoRes.class));
        if(awbResponse.getMeta().getAwbRoutingInfo() != null && !awbResponse.getMeta().getAwbRoutingInfo().isEmpty()){
            for (var awbRoute: awbResponse.getMeta().getAwbRoutingInfo()){
                unlocoRequests.add(awbRoute.getOriginPortName());
                unlocoRequests.add(awbRoute.getDestinationPortName());
                carrierRequests.add(awbRoute.getByCarrier());
            }
        }

        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        Map<String, EntityTransferCarrier> carriersMap = masterDataUtils.fetchInBulkCarriers(carrierRequests);

        if(unlocationsMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            var unloc = unlocationsMap.get(shipmentDetails.getCarrierDetails().getOriginPort());
            awbResponse.getMeta().setPol(populateUnlocFields(unloc));
        }
        if(unlocationsMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            var unloc = unlocationsMap.get(shipmentDetails.getCarrierDetails().getDestinationPort());
            awbResponse.getMeta().setPod(populateUnlocFields(unloc));
        }
        if(awbResponse.getMeta().getAwbRoutingInfo() != null && !awbResponse.getMeta().getAwbRoutingInfo().isEmpty()){
            for (var awbRoute: awbResponse.getMeta().getAwbRoutingInfo()){
                if(unlocationsMap.containsKey(awbRoute.getOriginPortName())) {
                    var unloc = unlocationsMap.get(awbRoute.getOriginPortName());
                    awbRoute.setOriginIATACode(unloc.getIataCode());
                }
                if(unlocationsMap.containsKey(awbRoute.getDestinationPortName())) {
                    var unloc = unlocationsMap.get(awbRoute.getDestinationPortName());
                    awbRoute.setDestinationIATACode(unloc.getIataCode());
                }
                if(carriersMap.containsKey(awbRoute.getByCarrier())) {
                    var carrier = carriersMap.get(awbRoute.getByCarrier());
                    awbRoute.setAirlineInfo(AwbAirMessagingResponse.AirlineInfo.builder()
                            .airlinePrefix(carrier.getAirlineCode())
                            .iata(carrier.getIATACode())
                            .build());
                }
            }
        }
        if(awbResponse.getAwbPaymentInfo() != null) {
            if (!Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalCollect()) && !Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalPrepaid()))
                awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalCollect().max(awbResponse.getAwbPaymentInfo().getTotalPrepaid()));
            else if (!Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalCollect()))
                awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalCollect());
            else if (!Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalPrepaid()))
                awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalPrepaid());
        }
        awbResponse.getMeta().setTenantInfo(populateTenantInfoFields(tenantModel, shipmentSettingsDetails));
        if (!Objects.isNull(awbResponse.getAwbCargoInfo()) && StringUtility.isNotEmpty(awbResponse.getAwbCargoInfo().getCustomOriginCode()))
            awbResponse.getMeta().setCustomOriginCode(CountryListHelper.ISO3166.fromAlpha3(awbResponse.getAwbCargoInfo().getCustomOriginCode()).getAlpha2());
        return awbResponse;
    }
}
