package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingEventDto;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingStatusDto;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.impl.ShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbAddressParam;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.google.common.base.Strings;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.mail.MessagingException;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("rawtypes")
@Component
@Slf4j
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
    @Autowired
    @Lazy
    private IAwbDao awbDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IAirMessagingLogsDao airMessagingLogsDao;
    @Autowired
    private IEventDao eventDao;
    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private IAirMessagingLogsService airMessagingLogsService;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private CommonUtils commonUtils;
    private ShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    public void setShipmentSettingsDao(ShipmentSettingsDao shipmentSettingsDao) { this.shipmentSettingsDao = shipmentSettingsDao; }

    public static String getFormattedAddress(AwbAddressParam addressParam)
    {
        String forMattedAddress = "";
        String newLine = "\r\n";
        if (!StringUtility.isEmpty(addressParam.getAddress1()))
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

    public static String constructAddressForAwb(Map<String, Object> addressData) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";

        if(addressData != null) {
            if (addressData.containsKey(PartiesConstants.ADDRESS1))
                sb.append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS1)));
            if (addressData.containsKey(PartiesConstants.ADDRESS2)) {
                if(!sb.isEmpty()) sb.append(newLine);
                sb.append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS2)));
            }
        }

        return sb.toString();
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
        if (Objects.equals(shipmentDetails.getJobType(), ShipmentConstants.SHIPMENT_TYPE_DRT) && (shipmentDetails.getMasterBill() == null || shipmentDetails.getMasterBill().isBlank())) {
            throw new ValidationException("MAWB Number is required in shipment to generate the document.");
        }
        if (shipmentDetails.getHouseBill() == null || shipmentDetails.getHouseBill().isBlank()) {
            if(!(Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType()) && Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()))) {
                throw new ValidationException("HAWB Number is required in shipment to generate the document.");
            }
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


    public AwbAirMessagingResponse createAirMessagingRequestForConsole(Awb awb, ConsolidationDetails consolidationDetails) {
        TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        AwbAirMessagingResponse awbResponse = jsonHelper.convertValue(awb, AwbAirMessagingResponse.class);
        awbResponse.setMeta(AwbAirMessagingResponse.Meta.builder().build());
        this.populateEnums(awbResponse);

        List<Parties> orgLists = new ArrayList<>();
        Parties issuingAgent = null;
        for (var orgRow : consolidationDetails.getConsolidationAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
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

                CommonV1ListRequest addressRequest = new CommonV1ListRequest();
                List<Object> addressField = new ArrayList<>(List.of("OrgId"));
                List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", orgs.get(0).getId()));
                addressRequest.setCriteriaRequests(addressCriteria);
                V1DataResponse addressResponse = v1Service.addressList(addressRequest);
                List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);

                String number = null;
                String expiry = null;
                String postCode = null;
                if(addressList != null && !addressList.isEmpty()){
                    EntityTransferAddress address = addressList.stream().filter(x -> Objects.equals(x.getAddressShortCode(), "Default")).findFirst().orElse(addressList.get(0));
                    if(address != null) {
                        number = address.getKCRANumber();
                        expiry = address.getKCRAExpiry();
                        postCode = address.getZipPostCode();
                    }
                }
                awbResponse.getMeta().setIssueingAgent(AwbAirMessagingResponse.OrgDetails.builder()
                        .city(awb.getAwbShipmentInfo().getIssuingAgentCity())
                        .country(CountryListHelper.ISO3166.getAlpha2IfAlpha3(awb.getAwbShipmentInfo().getIssuingAgentCountry()))
                        .currency(orgs.get(0).getCurrencyCode())
                        .expiry(expiry != null ? LocalDateTime.parse(expiry) : null)
                        .number(number)
                        .postCode(awb.getAwbShipmentInfo().getIssuingAgentZipCode())
                        .build());
            }
        } else {
            if(response.getOrganizations().containsKey(issuingAgent.getOrgCode())
                    && response.getAddresses().containsKey(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode())) {
                var org = response.getOrganizations().get(issuingAgent.getOrgCode());
                var address = response.getAddresses().get(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode());
                awbResponse.getMeta().setIssueingAgent(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getIssuingAgentCountry(), awb.getAwbShipmentInfo().getIssuingAgentCity(), awb.getAwbShipmentInfo().getIssuingAgentZipCode()));
            }
        }

        if(consolidationDetails.getSendingAgent() != null && (response.getOrganizations().containsKey(consolidationDetails.getSendingAgent().getOrgCode())
             && response.getAddresses().containsKey(consolidationDetails.getSendingAgent().getOrgCode() + '#' + consolidationDetails.getSendingAgent().getAddressCode()))){
                var org = response.getOrganizations().get(consolidationDetails.getSendingAgent().getOrgCode());
                var address = response.getAddresses().get(consolidationDetails.getSendingAgent().getOrgCode() + '#' + consolidationDetails.getSendingAgent().getAddressCode());
                awbResponse.getMeta().setShipper(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getShipperCountry(), awb.getAwbShipmentInfo().getShipperCity(), awb.getAwbShipmentInfo().getShipperZipCode()));

        }
        if(consolidationDetails.getReceivingAgent() != null && (response.getOrganizations().containsKey(consolidationDetails.getReceivingAgent().getOrgCode())
                    && response.getAddresses().containsKey(consolidationDetails.getReceivingAgent().getOrgCode() + '#' + consolidationDetails.getReceivingAgent().getAddressCode()))){
                var org = response.getOrganizations().get(consolidationDetails.getReceivingAgent().getOrgCode());
                var address = response.getAddresses().get(consolidationDetails.getReceivingAgent().getOrgCode() + '#' + consolidationDetails.getReceivingAgent().getAddressCode());
                awbResponse.getMeta().setConsignee(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getConsigneeCountry(), awb.getAwbShipmentInfo().getConsigneeCity(), awb.getAwbShipmentInfo().getConsigneeZipCode()));

        }

        List<String> unlocoRequests = new ArrayList<>();
        Set<String> carrierRequests = new HashSet<>();

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
                    awbRoute.setOriginPortUnlocName(unloc.getName());
                }
                if(unlocationsMap.containsKey(awbRoute.getDestinationPortName())) {
                    var unloc = unlocationsMap.get(awbRoute.getDestinationPortName());
                    awbRoute.setDestinationIATACode(unloc.getIataCode());
                    awbRoute.setDestinationPortUnlocName(unloc.getName());
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

        if(awbResponse.getMeta() != null) {
            var user = UserContext.getUser();
            awbResponse.getMeta().setUserInfo(populateUserInfoFields(user));
            awbResponse.getMeta().setMasterAwbNumber(consolidationDetails.getBol());
        }

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
                .countyCode(CountryListHelper.ISO3166.getAlpha2IfAlpha3(unloc.getCountry()))
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
                .country(CountryListHelper.ISO3166.getAlpha2IfAlpha3(tenantModel.country))
                .state(tenantModel.state)
                .branchCode(tenantModel.code)
                .branchName(tenantModel.tenantName)
                .build();
    }

    private AwbAirMessagingResponse.OrgDetails populateOrgsFields(Map<String, Object> org, Map<String, Object> address, String country, String city, String zipCode) {

        return AwbAirMessagingResponse.OrgDetails.builder()
                        .city(city)
                        .country(CountryListHelper.ISO3166.getAlpha2IfAlpha3(country))
                        .currency(org.containsKey(PartiesConstants.CURRENCY_CODE) ? (String) org.get(PartiesConstants.CURRENCY_CODE) : null)
                        .expiry(address.containsKey(PartiesConstants.KC_RA_EXPIRY) && StringUtility.isNotEmpty((String)address.get(PartiesConstants.KC_RA_EXPIRY)) ? LocalDateTime.parse((String) address.get(PartiesConstants.KC_RA_EXPIRY)) : null)
                        .number(address.containsKey(PartiesConstants.KC_RA_NUMBER) ? (String) address.get(PartiesConstants.KC_RA_NUMBER) : null)
                        .postCode(zipCode)
                .build();
    }

    public AwbAirMessagingResponse createAirMessagingRequestForShipment(Awb awb, ShipmentDetails shipmentDetails, TenantModel tenantModel) {
        if (Objects.isNull(tenantModel))
            tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        var shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(shipmentDetails.getTenantId())).stream().findFirst().orElse(new ShipmentSettingsDetails());
        AwbAirMessagingResponse awbResponse = jsonHelper.convertValue(awb, AwbAirMessagingResponse.class);
        awbResponse.setMeta(AwbAirMessagingResponse.Meta.builder().build());
        this.populateEnums(awbResponse);

        List<Parties> orgLists = new ArrayList<>();
        Parties issuingAgent = null;
        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
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

                CommonV1ListRequest addressRequest = new CommonV1ListRequest();
                List<Object> addressField = new ArrayList<>(List.of("OrgId"));
                List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", orgs.get(0).getId()));
                addressRequest.setCriteriaRequests(addressCriteria);
                V1DataResponse addressResponse = v1Service.addressList(addressRequest);
                List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);

                String number = null;
                String expiry = null;
                String postCode = null;
                if(addressList != null && !addressList.isEmpty()){
                    EntityTransferAddress address = addressList.stream().filter(x -> Objects.equals(x.getAddressShortCode(), "Default")).findFirst().orElse(addressList.get(0));
                    if(address != null) {
                        number = address.getKCRANumber();
                        expiry = address.getKCRAExpiry();
                        postCode = address.getZipPostCode();
                    }
                }
                awbResponse.getMeta().setIssueingAgent(AwbAirMessagingResponse.OrgDetails.builder()
                        .city(awb.getAwbShipmentInfo().getIssuingAgentCity())
                        .country(CountryListHelper.ISO3166.getAlpha2IfAlpha3(awb.getAwbShipmentInfo().getIssuingAgentCountry()))
                        .currency(orgs.get(0).getCurrencyCode())
                        .expiry(expiry != null ? LocalDateTime.parse(expiry): null)
                        .number(number)
                        .postCode(awb.getAwbShipmentInfo().getIssuingAgentZipCode())
                        .build());
            }
        } else {
            if(response.getOrganizations().containsKey(issuingAgent.getOrgCode())
                    && response.getAddresses().containsKey(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode())) {
                var org = response.getOrganizations().get(issuingAgent.getOrgCode());
                var address = response.getAddresses().get(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode());
                awbResponse.getMeta().setIssueingAgent(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getIssuingAgentCountry(), awb.getAwbShipmentInfo().getIssuingAgentCity(), awb.getAwbShipmentInfo().getIssuingAgentZipCode()));
            }
        }

        if(shipmentDetails.getConsigner() != null && (response.getOrganizations().containsKey(shipmentDetails.getConsigner().getOrgCode())
                    && response.getAddresses().containsKey(shipmentDetails.getConsigner().getOrgCode() + '#' + shipmentDetails.getConsigner().getAddressCode()))){
                var org = response.getOrganizations().get(shipmentDetails.getConsigner().getOrgCode());
                var address = response.getAddresses().get(shipmentDetails.getConsigner().getOrgCode() + '#' + shipmentDetails.getConsigner().getAddressCode());
                awbResponse.getMeta().setShipper(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getShipperCountry(), awb.getAwbShipmentInfo().getShipperCity(), awb.getAwbShipmentInfo().getShipperZipCode()));

        }
        if(shipmentDetails.getConsignee() != null && (response.getOrganizations().containsKey(shipmentDetails.getConsignee().getOrgCode())
                    && response.getAddresses().containsKey(shipmentDetails.getConsignee().getOrgCode() + '#' + shipmentDetails.getConsignee().getAddressCode()))) {
                var org = response.getOrganizations().get(shipmentDetails.getConsignee().getOrgCode());
                var address = response.getAddresses().get(shipmentDetails.getConsignee().getOrgCode() + '#' + shipmentDetails.getConsignee().getAddressCode());
                awbResponse.getMeta().setConsignee(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getConsigneeCountry(), awb.getAwbShipmentInfo().getConsigneeCity(), awb.getAwbShipmentInfo().getConsigneeZipCode()));

        }

        List<String> unlocoRequests = new ArrayList<>();
        Set<String> carrierRequests = new HashSet<>();

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
                    awbRoute.setOriginPortUnlocName(unloc.getName());
                }
                if(unlocationsMap.containsKey(awbRoute.getDestinationPortName())) {
                    var unloc = unlocationsMap.get(awbRoute.getDestinationPortName());
                    awbRoute.setDestinationIATACode(unloc.getIataCode());
                    awbRoute.setDestinationPortUnlocName(unloc.getName());
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

        if(awbResponse.getMeta() != null) {
            var user = UserContext.getUser();

            awbResponse.getMeta().setUserInfo(populateUserInfoFields(user));
            awbResponse.getMeta().setMasterAwbNumber(shipmentDetails.getMasterBill());
        }
        if (!Objects.isNull(awbResponse.getAwbCargoInfo()) && StringUtility.isNotEmpty(awbResponse.getAwbCargoInfo().getCustomOriginCode())) {
            String countryCode = awbResponse.getAwbCargoInfo().getCustomOriginCode();
            awbResponse.getMeta().setCustomOriginCode(!StringUtility.isNotEmpty(countryCode) && countryCode.length() == 3 ? CountryListHelper.ISO3166.fromAlpha3(awbResponse.getAwbCargoInfo().getCustomOriginCode()).getAlpha2() : awbResponse.getAwbCargoInfo().getCustomOriginCode());
        }
        return awbResponse;
    }

    private AwbAirMessagingResponse.UserInfo populateUserInfoFields(UsersDto user) {
        return AwbAirMessagingResponse.UserInfo.builder()
                .userName(user.getUsername())
                .build();
    }

    public void createStatusUpdateForAirMessaging(AirMessagingStatusDto airMessageStatus) throws RunnerException, MessagingException, IOException {
        var guid = airMessageStatus.getGuid();
        log.info("Air-messaging : entered createStatusUpdateForAirMessaging; guid {}", guid);
        Optional<Awb> awb = Optional.ofNullable(awbDao.findAwbByGuidByQuery(guid));
        if(awb.isEmpty()){
            throw new RunnerException("No Awb exist for given Guid: " + guid);
        }
        var tenantId = awb.get().getTenantId();

        AirMessagingStatus status;
        if(Objects.equals(airMessageStatus.getStatus(), "INTERNAL_VALIDATION_ERROR") || Objects.equals(airMessageStatus.getStatus(), "EXTERNAL_VALIDATION_ERROR")
        || Objects.equals(airMessageStatus.getStatus(), "INTERNAL_ERROR") || Objects.equals(airMessageStatus.getStatus(), "REJECTED")) {
            status = AirMessagingStatus.FAILED;
        } else if (Objects.equals(airMessageStatus.getStatus(), "INITIATED") || Objects.equals(airMessageStatus.getStatus(), "SUBMITTED")) {
            status = AirMessagingStatus.SUBMITTED;
        } else if (Objects.equals(airMessageStatus.getStatus(), "RECEIVED")){
            status = AirMessagingStatus.SUCCESS;
        } else if (Objects.equals(airMessageStatus.getStatus(), AirMessagingLogsConstants.PROCESSED)) {
            status = AirMessagingStatus.SUCCESS_BY_CARRIER;
        } else {
            throw new RunnerException("This status is not accepted by runner");
        }

        String xmlPayload = airMessageStatus.getXmlPayload() != null ? new String(Base64.decodeBase64(airMessageStatus.getXmlPayload()), StandardCharsets.UTF_8) : null;


        switch (status) {
            case FAILED -> awbDao.updateAirMessageStatus(guid, AwbStatus.AIR_MESSAGE_FAILED.name());
            case SUBMITTED -> awbDao.updateAirMessageStatus(guid, AwbStatus.AIR_MESSAGE_SENT.name());
            case SUCCESS -> awbDao.updateAirMessageStatus(guid, AwbStatus.AIR_MESSAGE_SUCCESS.name());
        }
        Awb masterAwb = null;
        if(awb.get().getConsolidationId() != null) {
            airMessagingLogsDao.createAirMessagingLogs(UUID.randomUUID(), guid, airMessageStatus.getErrorMessage(),
                    airMessageStatus.getMessageType() != null ? airMessageStatus.getMessageType() : "FWB", xmlPayload,
                    status.name(), tenantId, LocalDateTime.now());
            masterAwb = awb.get();
            eventDao.createEventForAirMessagingStatus(UUID.randomUUID(), awb.get().getConsolidationId(),
                    Constants.CONSOLIDATION, "FNM", "FNM received", LocalDateTime.now(), LocalDateTime.now(),
                    Constants.DESCARTES, tenantId, status.name(), LocalDateTime.now(), LocalDateTime.now());

            List<Awb> awbsList = awbDao.findAllLinkedAwbs(guid);

            AwbStatus hawbsStatus = null;
            Boolean allStatusReceived = true;
            if(awbsList != null && !awbsList.isEmpty()){
                for (var x: awbsList) {
                    if(x.getShipmentId() != null){
                        if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
                            hawbsStatus = AwbStatus.AIR_MESSAGE_SENT;
                        } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_FAILED) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT)) {
                            hawbsStatus = AwbStatus.AIR_MESSAGE_FAILED;
                        } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SUCCESS) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED)) {
                            hawbsStatus = AwbStatus.AIR_MESSAGE_SUCCESS;
                        }
                    }
                    if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
                        allStatusReceived = false;
                    }
                }
            }
            if(hawbsStatus != null)
                awbDao.updateLinkedHawbAirMessageStatus(guid, hawbsStatus.name());

            if(Boolean.TRUE.equals(allStatusReceived) && (Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED) || Objects.equals(status, AwbStatus.AIR_MESSAGE_FAILED))) {
                try {
                    this.sendAirMessagingFailureEmail(masterAwb, awbsList);
                } catch (Exception e) {
                    log.error("Send Email for Air Messaging Failure : " + e.getMessage());
                }
            }


        } else if (awb.get().getShipmentId() != null) {
            var shipmentDetailsList = shipmentDao.getShipmentNumberFromId(List.of(awb.get().getShipmentId()));
            String msgType = airMessageStatus.getMessageType();
            if(shipmentDetailsList != null && !shipmentDetailsList.isEmpty()) {
                if(msgType == null){
                    if(Objects.equals(shipmentDetailsList.get(0).getJobType(), Constants.SHIPMENT_TYPE_DRT))
                        msgType = "FWB";
                    else
                        msgType = "FZB";
                }
            }
            airMessagingLogsDao.createAirMessagingLogs(UUID.randomUUID(), guid, airMessageStatus.getErrorMessage(),
                    msgType, xmlPayload, status.name(), tenantId, LocalDateTime.now());
            eventDao.createEventForAirMessagingStatus(UUID.randomUUID(), awb.get().getShipmentId(),
                    Constants.SHIPMENT, "FNM", "FNM received", LocalDateTime.now(), LocalDateTime.now(),
                    Constants.DESCARTES, tenantId, status.name(), LocalDateTime.now(), LocalDateTime.now());

            List<Awb> awbsList = awbDao.findAllLinkedAwbs(guid);

            AwbStatus hawbsStatus = null;
            UUID consoleGuid = null;
            Boolean allStatusReceived = true;
            AwbStatus consoleStatus = null;

            if(awbsList != null && !awbsList.isEmpty()){
                for (var x: awbsList) {
                    if(x.getShipmentId() != null){
                        if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
                            hawbsStatus = AwbStatus.AIR_MESSAGE_SENT;
                        } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_FAILED) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT)) {
                            hawbsStatus = AwbStatus.AIR_MESSAGE_FAILED;
                        } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SUCCESS) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED)) {
                            hawbsStatus = AwbStatus.AIR_MESSAGE_SUCCESS;
                        }
                    } else if (x.getConsolidationId() != null) {
                        consoleGuid = x.getGuid();
                        consoleStatus = x.getAirMessageStatus();
                        masterAwb = x;
                    }
                    if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
                        allStatusReceived = false;
                    }
                }
                if(awbsList.size() == 1){
                    masterAwb = awbsList.get(0);
                }
            }
            if(masterAwb == null) {
                consoleStatus = awb.get().getAirMessageStatus();
                masterAwb = awb.get();
            }
            if(hawbsStatus != null && consoleGuid != null)
                awbDao.updateLinkedHawbAirMessageStatus(consoleGuid, hawbsStatus.name());

            if(Boolean.TRUE.equals(allStatusReceived) && (Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED) || Objects.equals(consoleStatus, AwbStatus.AIR_MESSAGE_FAILED))) {
                try {
                    this.sendAirMessagingFailureEmail(masterAwb, awbsList);
                } catch (Exception e) {
                    log.error("Send Email for Air Messaging Failure : " + e.getMessage());
                }
            }

        }
    }

    public void createEventUpdateForAirMessaging(AirMessagingEventDto airMessageEvent) throws RunnerException {
        var guid = airMessageEvent.getGuid();
        log.info("Air-messaging : entered createEventUpdateForAirMessaging; guid : {}", guid);
        var awb = awbDao.findByGuid(guid);
        if(awb.isEmpty()){
            throw new RunnerException("No Awb exist for given Guid: " + guid);
        }
        var tenantId = awb.get().getTenantId();
        List<ConsoleShipmentMapping> consoleShipmentMappings = null;
        if(awb.get().getConsolidationId() != null) {
            consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationIdByQuery(awb.get().getConsolidationId());

            eventDao.createEventForAirMessagingEvent(prepareEventPayload(airMessageEvent, awb.get().getConsolidationId(), Constants.CONSOLIDATION, tenantId));
            if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()){
                consoleShipmentMappings.forEach(x -> eventDao.createEventForAirMessagingEvent(prepareEventPayload(airMessageEvent, x.getShipmentId(), Constants.SHIPMENT, tenantId)));
            }
        } else if (awb.get().getShipmentId() != null) {
            eventDao.createEventForAirMessagingEvent(prepareEventPayload(airMessageEvent, awb.get().getShipmentId(), Constants.SHIPMENT, tenantId));
        }
        updateAwbStatusForFsuUpdate(awb.get(), airMessageEvent.getEventCode(), consoleShipmentMappings);

    }

    private Events prepareEventPayload(AirMessagingEventDto airMessageEvent, Long entityId, String entityType, Integer tenantId) {
        Events events = new Events();
        events.setGuid(UUID.randomUUID());
        events.setEntityId(entityId);
        events.setEntityType(entityType);
        events.setTenantId(tenantId);
        events.setEventCode(airMessageEvent.getEventCode());
        events.setDescription(airMessageEvent.getEventCode());
        events.setSource(Constants.DESCARTES);
        events.setPieces(airMessageEvent.getPieces());
        events.setTotalPieces(airMessageEvent.getTotalPieces());
        events.setWeight(airMessageEvent.getWeight());
        events.setTotalWeight(airMessageEvent.getTotalWeight());
        events.setIsPartial(Objects.equals(airMessageEvent.getIsPartial(), "P"));
        events.setCreatedAt(LocalDateTime.now());
        events.setUpdatedAt(LocalDateTime.now());
        events.setActual(airMessageEvent.getActualTimeOfEvent());
        events.setEstimated(airMessageEvent.getEstimatedTimeOfEvent());
        events.setReceivedDate(airMessageEvent.getTimeOfReceivingFSUMessage());
        events.setScheduledDate(airMessageEvent.getScheduledTimeOfEvent());
        events.setPlaceName(airMessageEvent.getPlaceOfEvent());
        events.setPlaceDescription(airMessageEvent.getPlaceDescription());
        events.setLatitude(airMessageEvent.getLatitude());
        events.setLongitude(airMessageEvent.getLongitude());
        return events;
    }

    public void sendAirMessagingFailureEmail(Awb awb, List<Awb> awbsList) throws MessagingException, IOException {
        if(awb == null)
            return;
        String mawbNumber = awb.getAwbNumber();
        String userName = awb.getUserDisplayName();
        String entityName = awb.getConsolidationId() != null ? "consolidation" : "shipment";
        String entityNumber = "";
        if(awb.getShipmentId() != null) {
            var shipmentDetailsList = shipmentDao.getShipmentNumberFromId(List.of(awb.getShipmentId()));
            if(shipmentDetailsList != null && !shipmentDetailsList.isEmpty())
                entityNumber = shipmentDetailsList.get(0).getShipmentId();
        }
        if(awb.getConsolidationId() != null) {
            entityNumber = consolidationDetailsDao.getConsolidationNumberFromId(awb.getConsolidationId());
        }

        List<String> emailIds = new ArrayList<>();
        emailIds.add(awb.getUserMailId());
        AirMessagingLogs masterAirMessagingLogs = airMessagingLogsService.getRecentLogForEntityGuid(awb.getGuid());
        String subject = "Air message failure notification for "+ mawbNumber;
        String body = "Dear " + userName +",\n \n \n" +
                "Please find below the status of air messages for \""+ mawbNumber + "\" associated with " + entityName + " \""+ entityNumber + "\". \n\n";

        if(masterAirMessagingLogs != null) {
            if(Objects.equals(masterAirMessagingLogs.getStatus(), AirMessagingStatus.SUCCESS.name())) {
                body = body + "FWB for \""+ mawbNumber + "\"/\"" + entityNumber +"\" : Success\n";
            } else if (Objects.equals(masterAirMessagingLogs.getStatus(), AirMessagingStatus.FAILED.name())) {
                body = body + "FWB for \""+ mawbNumber + "\"/\"" + entityNumber +"\" : Failed. Failure reason is \""+ masterAirMessagingLogs.getErrorMessage() +"\"\n\n";
            }
        }
        if(!awbsList.isEmpty() && awbsList.size() > 1) {
            var shipmentIds = awbsList.stream().filter(x -> x.getShipmentId() != null).map(Awb::getShipmentId).toList();
            if(shipmentIds != null) {
                var shipmentDetailsList = shipmentDao.getShipmentNumberFromId(shipmentIds);
                Map<Long, String> map = shipmentDetailsList.stream().collect(Collectors.toMap(ShipmentDetails::getId, ShipmentDetails::getShipmentId));
                for (var x : awbsList) {
                    if (x.getShipmentId() != null) {
                        String shipNumber = map.get(x.getShipmentId());
                        AirMessagingLogs shipAirMessagingLogs = airMessagingLogsService.getRecentLogForEntityGuid(x.getGuid());
                        if (shipAirMessagingLogs != null) {
                            if (Objects.equals(shipAirMessagingLogs.getStatus(), AirMessagingStatus.SUCCESS.name())) {
                                body = body + "FZB for \"" + shipNumber + "\" : Success\n";
                            } else if (Objects.equals(shipAirMessagingLogs.getStatus(), AirMessagingStatus.FAILED.name())) {
                                body = body + "FZB for \"" + shipNumber + "\" : Failed. Failure reason is \"" + shipAirMessagingLogs.getErrorMessage() + "\"\n\n";
                            }
                        }
                    }
                }
            }
        }
        body = body + "\n \n Please rectify the data as per the failure comment and resend the messages by clicking on the \"Print\" button in the MAWB. \n \n" +
                        "Thank you!\n" + "Cargoes Runner";
        emailServiceUtility.sendEmail(body, subject, emailIds, null, null, null);
    }

    private void updateAwbStatusForFsuUpdate(Awb awb, String eventCode, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        if(!AwbConstants.FSU_LOCK_EVENT_CODE.equalsIgnoreCase(eventCode))
            return;

        AwbStatus status = AwbStatus.AWB_FSU_LOCKED;
        if(awb.getConsolidationId() != null) {
            awbDao.updateLinkedHawbAirMessageStatus(awb.getGuid(), status.name());
            if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()){
                consoleShipmentMappings.forEach(x -> awbDao.updateAirMessageStatusFromShipmentId(x.getShipmentId() , status.name()));
            }
        }
        awbDao.updateAirMessageStatus(awb.getGuid(), status.name());
    }

    public void overrideInfoForCoLoadShipment(AwbAirMessagingResponse awbResponse, boolean override) {
        if (Boolean.TRUE.equals(override)) {
            var tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            awbResponse.getMeta().getTenantInfo().setPimaAddress(tenantModel.getPIMAAddress());
        }
    }
}
