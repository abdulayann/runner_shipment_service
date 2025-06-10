package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.response.AwbRoutingInfoResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingEventDto;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingStatusDto;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.impl.ShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
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
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.convertToDPWDateFormatWithTime;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

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
        return new BigDecimal(charge); //NOSONAR
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
        if (isHawbNumberPresent(shipmentDetails)) {
            throw new ValidationException("HAWB Number is required in shipment to generate the document.");
        }

    }

    private static boolean isHawbNumberPresent(ShipmentDetails shipmentDetails) {
        return (shipmentDetails.getHouseBill() == null || shipmentDetails.getHouseBill().isBlank()) && !(Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType()) && Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()));
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
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        AwbAirMessagingResponse awbResponse = jsonHelper.convertValue(awb, AwbAirMessagingResponse.class);
        awbResponse.setMeta(AwbAirMessagingResponse.Meta.builder().build());
        this.populateEnums(awbResponse);
        checkAcasFlagInAwb(awbResponse);
        awbResponse.getMeta().setWeightDecimalPlaces(Objects.isNull(v1TenantSettingsResponse.getWeightDecimalPlace()) ? 2 : v1TenantSettingsResponse.getWeightDecimalPlace());
        awbResponse.getMeta().setCurrencyDecimalPlaces(Objects.isNull(v1TenantSettingsResponse.getCurrencyDecimalPlace()) ? 2 : v1TenantSettingsResponse.getCurrencyDecimalPlace());
        awbResponse.getMeta().setVolumeDecimalPlaces(Objects.isNull(v1TenantSettingsResponse.getVolumeDecimalPlace()) ? 3 : v1TenantSettingsResponse.getVolumeDecimalPlace());
        // Populate Special handling codes master data
        this.populateMasterDataMap(awbResponse, awb);


        Parties issuingAgent = getIssuingAgentForConsole(consolidationDetails);
        List<Parties> orgLists = getOrgListsForConsole(consolidationDetails, issuingAgent);
        OrgAddressResponse response = v1ServiceUtil.fetchOrgInfoFromV1(orgLists);
        if(issuingAgent == null){
            processEmptyIssuingAgent(awb, tenantModel, awbResponse);
        } else {
            setIssuingAgentInResponse(awb, response, issuingAgent, awbResponse);
        }

        setShipperAndConsigneeForConsole(awb, consolidationDetails, response, awbResponse);

        Set<String> carrierRequests = new HashSet<>();

        List<String> unlocoRequests = getUnLocRequestsForConsole(awb, consolidationDetails);
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

        this.buildRcpIATAMapAndNotifyParty(awbResponse, unlocationsMap);

        processConsoleUnLoc(awb, consolidationDetails, unlocationsMap, awbResponse);
        processAwbRoutingInfo(awbResponse, unlocationsMap, carriersMap);
        if(checkAwbPaymentInfoForNullValues(awbResponse))
            awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalCollect().max(awbResponse.getAwbPaymentInfo().getTotalPrepaid()));
        awbResponse.getMeta().setTenantInfo(populateTenantInfoFields(tenantModel, shipmentSettingsDetails));
        awbResponse.getMeta().setAdditionalSecurityInfo(consolidationDetails.getAdditionalSecurityInformation());
        if(awbResponse.getAwbCargoInfo() != null && StringUtility.isNotEmpty(awbResponse.getAwbCargoInfo().getCsdInfo()) && StringUtility.isEmpty(awbResponse.getAwbCargoInfo().getCsdInfoDate())) {
            awbResponse.getAwbCargoInfo().setCsdInfoDate(convertToDPWDateFormatWithTime(awb.getOriginalPrintedAt(), v1TenantSettingsResponse.getDPWDateFormat(), true, true));
        }

        if(awbResponse.getMeta() != null) {
            var user = UserContext.getUser();
            awbResponse.getMeta().setUserInfo(populateUserInfoFields(user));
            awbResponse.getMeta().setMasterAwbNumber(consolidationDetails.getBol());
            awbResponse.getMeta().setEntityNumber(consolidationDetails.getConsolidationNumber());
        }

        // Rounding off Currencies fields
        this.roundOffCurrencyFields(awbResponse);

        // Rounding off Weight fields
        this.roundOffWeightFields(awbResponse);
        this.roundOffVolumeFields(awbResponse);

        return awbResponse;
    }

    private boolean checkAwbPaymentInfoForNullValues(AwbAirMessagingResponse awbResponse) {
        return awbResponse.getAwbPaymentInfo() != null && awbResponse.getAwbPaymentInfo().getTotalCollect() != null
                && awbResponse.getAwbPaymentInfo().getTotalPrepaid() != null;
    }

    private void setShipperAndConsigneeForConsole(Awb awb, ConsolidationDetails consolidationDetails, OrgAddressResponse response, AwbAirMessagingResponse awbResponse) {
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
    }

    private void processConsoleUnLoc(Awb awb, ConsolidationDetails consolidationDetails, Map<String, UnlocationsResponse> unlocationsMap, AwbAirMessagingResponse awbResponse) {
        if(awb.getAwbPackingInfo() != null && unlocationsMap.containsKey(awb.getAwbOtherInfo().getExecutedAt())) {
            var unloc = unlocationsMap.get(awb.getAwbOtherInfo().getExecutedAt());
            awbResponse.getMeta().setExecutedAtCity(unloc.getNameWoDiacritics());
        }
        if(unlocationsMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            var unloc = unlocationsMap.get(consolidationDetails.getCarrierDetails().getOriginPort());
            awbResponse.getMeta().setPol(populateUnlocFields(unloc));
        }
        if(unlocationsMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            var unloc = unlocationsMap.get(consolidationDetails.getCarrierDetails().getDestinationPort());
            awbResponse.getMeta().setPod(populateUnlocFields(unloc));
        }
    }

    private Parties getIssuingAgentForConsole(ConsolidationDetails consolidationDetails) {
        Parties issuingAgent = null;
        for (var orgRow : consolidationDetails.getConsolidationAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                issuingAgent = orgRow;
            }
        }
        return issuingAgent;
    }

    private List<Parties> getOrgListsForConsole(ConsolidationDetails consolidationDetails, Parties issuingAgent) {
        List<Parties> orgLists = new ArrayList<>();
        if(consolidationDetails.getSendingAgent() != null){
            orgLists.add(consolidationDetails.getSendingAgent());
        }
        if(consolidationDetails.getReceivingAgent() != null){
            orgLists.add(consolidationDetails.getReceivingAgent());
        }
        if(issuingAgent != null) {
            orgLists.add(issuingAgent);
        }
        return orgLists;
    }

    private List<String> getUnLocRequestsForConsole(Awb awb, ConsolidationDetails consolidationDetails) {
        List<String> unlocoRequests = new ArrayList<>();
        awb.getAwbNotifyPartyInfo().forEach(party -> {
            if(party.getSpecifiedAddressLocation() != null)
                unlocoRequests.add(party.getSpecifiedAddressLocation());
        });
        if(awb.getAwbOtherInfo() != null && awb.getAwbOtherInfo().getExecutedAt() != null)
            unlocoRequests.add(awb.getAwbOtherInfo().getExecutedAt());

        unlocoRequests.add(consolidationDetails.getCarrierDetails().getOriginPort());
        unlocoRequests.add(consolidationDetails.getCarrierDetails().getDestinationPort());
        return unlocoRequests;
    }

    private void buildRcpIATAMapAndNotifyParty(AwbAirMessagingResponse awbResponse, Map<String, UnlocationsResponse> unlocationsMap) {
        if (awbResponse.getAwbNotifyPartyInfo() != null) {
            awbResponse.getAwbNotifyPartyInfo().forEach(party -> {
                if (party.getSpecifiedAddressLocation() != null && unlocationsMap.containsKey(party.getSpecifiedAddressLocation())) {
                    party.setSpecifiedAddressLocationName(unlocationsMap.get(party.getSpecifiedAddressLocation()).getName());
                    party.setSpecifiedAddressLocationIATACode(unlocationsMap.get(party.getSpecifiedAddressLocation()).getIataCode());
                }
            });
        }
    }

    private void populateMasterDataMap(AwbAirMessagingResponse awbResponse, Awb awb) {
        MasterListRequestV2 requests = new MasterListRequestV2();
        List<MasterListRequest> masterListRequests = new ArrayList<>();
        awb.getAwbSpecialHandlingCodesMappings().forEach(x -> masterListRequests.add(MasterListRequest.builder().ItemValue(x.getShcId()).ItemType(MasterDataType.SPECIAL_HANDLING_CODES.getDescription()).build()));

        if (!masterListRequests.isEmpty()) {
            requests.setMasterListRequests(masterListRequests);
            requests.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));
            List<EntityTransferMasterLists> masterDataList = masterDataUtils.fetchMultipleMasterData(requests);
            Map<String, String> sphCodeMap = new HashMap<>();
            masterDataList.forEach(x -> sphCodeMap.put(x.ItemValue, x.ItemDescription));
            awbResponse.getMeta().setSchCodes(sphCodeMap);
        }
    }

    private void populateOtherPartyInfoList(AwbAirMessagingResponse awbResponse) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if (!Boolean.TRUE.equals(shipmentSettingsDetails.getIsAwbRevampEnabled()) || awbResponse == null) {
            return;
        }

        if (awbResponse.getAirMessagingAdditionalFields() == null) {
            awbResponse.setAirMessagingAdditionalFields(AirMessagingAdditionalFields.builder().otherPartyInfo(new ArrayList<>()).build());
        }

        List<OtherPartyInfo> otherPartyInfoList = awbResponse.getAirMessagingAdditionalFields().getOtherPartyInfo();
        if (otherPartyInfoList == null) {
            otherPartyInfoList = new ArrayList<>();
            awbResponse.getAirMessagingAdditionalFields().setOtherPartyInfo(otherPartyInfoList);
        }

        addOtherPartyInfo(awbResponse.getAwbShipmentInfo() != null ? awbResponse.getAwbShipmentInfo().getShipperPartyInfo() : null, OtherPartyType.SHIPPER, otherPartyInfoList);
        addOtherPartyInfo(awbResponse.getAwbShipmentInfo() != null ? awbResponse.getAwbShipmentInfo().getConsigneePartyInfo() : null, OtherPartyType.CONSIGNEE, otherPartyInfoList);
        addOtherPartyInfo(awbResponse.getAwbShipmentInfo() != null ? awbResponse.getAwbShipmentInfo().getIssuingAgentPartyInfo() : null, OtherPartyType.ISSUING_AGENT, otherPartyInfoList);

        List<AwbNotifyPartyInfo> notifyPartyInfoList = awbResponse.getAwbNotifyPartyInfo();
        if (notifyPartyInfoList != null && !notifyPartyInfoList.isEmpty()) {
            addOtherPartyInfo(notifyPartyInfoList.get(0).getOtherPartyInfo(), OtherPartyType.NOTIFY, otherPartyInfoList);
        }
    }

    private void addOtherPartyInfo(OtherPartyInfo partyInfo, OtherPartyType partyType, List<OtherPartyInfo> otherPartyInfoList) {
        if (partyInfo == null) {
            return;
        }
        partyInfo.setParty(partyType);
        otherPartyInfoList.add(partyInfo);
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
                .legalEntityName(commonUtils.getCurrentTenantSettings().getLegalEntityCode())
                .isShipmentV3Enabled(shipmentSettingsDetails.getIsRunnerV3Enabled())
                .build();
    }

    private AwbAirMessagingResponse.OrgDetails populateOrgsFields(Map<String, Object> org, Map<String, Object> address, String country, String city, String zipCode) {

        return AwbAirMessagingResponse.OrgDetails.builder()
                        .city(city)
                        .country(CountryListHelper.ISO3166.getAlpha2IfAlpha3(country))
                        .currency(org.containsKey(PartiesConstants.CURRENCY_CODE) ? (String) org.get(PartiesConstants.CURRENCY_CODE) : null)
                        .expiry(address.containsKey(PartiesConstants.KC_RA_EXPIRY) && StringUtility.isNotEmpty((String)address.get(PartiesConstants.KC_RA_EXPIRY)) ? LocalDateTime.parse((String) address.get(PartiesConstants.KC_RA_EXPIRY)) : null)
                        .number(address.containsKey(PartiesConstants.KC_RA_NUMBER) ? (String) address.get(PartiesConstants.KC_RA_NUMBER) : null)
                        .isKC(Boolean.TRUE.equals(address.get(PartiesConstants.KNOWN_CONSIGNOR)))
                        .isRA(Boolean.TRUE.equals(address.get(PartiesConstants.REGULATED_AGENT)))
                        .postCode(zipCode)
                .build();
    }

    public AwbAirMessagingResponse createAirMessagingRequestForShipment(Awb awb, ShipmentDetails shipmentDetails, TenantModel tenantModel, Awb masterAwb) throws RunnerException {
        if (Objects.isNull(tenantModel))
            tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        var shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(shipmentDetails.getTenantId())).stream().findFirst().orElse(new ShipmentSettingsDetails());
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        AwbAirMessagingResponse awbResponse = jsonHelper.convertValue(awb, AwbAirMessagingResponse.class);
        awbResponse.setMeta(AwbAirMessagingResponse.Meta.builder().build());
        this.populateEnums(awbResponse);
        checkAcasFlagInAwb(awbResponse);
        awbResponse.getMeta().setWeightDecimalPlaces(Objects.isNull(v1TenantSettingsResponse.getWeightDecimalPlace()) ? 2 : v1TenantSettingsResponse.getWeightDecimalPlace());
        awbResponse.getMeta().setCurrencyDecimalPlaces(Objects.isNull(v1TenantSettingsResponse.getCurrencyDecimalPlace()) ? 2 : v1TenantSettingsResponse.getCurrencyDecimalPlace());
        awbResponse.getMeta().setVolumeDecimalPlaces(Objects.isNull(v1TenantSettingsResponse.getVolumeDecimalPlace()) ? 3 : v1TenantSettingsResponse.getVolumeDecimalPlace());
        // Populate Special handling codes master data
        this.populateMasterDataMap(awbResponse, awb);
        this.populateOtherPartyInfoList(awbResponse);

        Parties issuingAgent = getIssuingAgent(shipmentDetails);
        List<Parties> orgLists = getOrgLists(shipmentDetails, issuingAgent);
        OrgAddressResponse response = v1ServiceUtil.fetchOrgInfoFromV1(orgLists);
        if(issuingAgent == null){
            processEmptyIssuingAgent(awb, tenantModel, awbResponse);
        } else {
            setIssuingAgentInResponse(awb, response, issuingAgent, awbResponse);
        }

        setShipperConsgineeDetailsInResponse(awb, shipmentDetails, response, awbResponse);

        Set<String> carrierRequests = new HashSet<>();

        List<String> unlocoRequests = getUnlocoRequests(awb, shipmentDetails);
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

        this.buildRcpIATAMapAndNotifyParty(awbResponse, unlocationsMap);

        processUnLocData(awb, shipmentDetails, unlocationsMap, awbResponse);
        processAwbRoutingInfo(awbResponse, unlocationsMap, carriersMap);
        processAwbPaymentInfo(awbResponse);
        awbResponse.getMeta().setTenantInfo(populateTenantInfoFields(tenantModel, shipmentSettingsDetails));
        awbResponse.getMeta().setAdditionalSecurityInfo(shipmentDetails.getAdditionalDetails().getAdditionalSecurityInformation());
        setUserDataInAwbResponseMeta(shipmentDetails, awbResponse);
        if(awbResponse.getAwbCargoInfo() != null && StringUtility.isNotEmpty(awbResponse.getAwbCargoInfo().getCsdInfo()) && StringUtility.isEmpty(awbResponse.getAwbCargoInfo().getCsdInfoDate())) {
            awbResponse.getAwbCargoInfo().setCsdInfoDate(convertToDPWDateFormatWithTime(awb.getOriginalPrintedAt(), v1TenantSettingsResponse.getDPWDateFormat(), true, true));
        }

        // Add MasterAwb details for FZB
        if(masterAwb != null) {
            this.populateMasterAwbData(awbResponse, masterAwb);
        }

        // Rounding off Currencies fields
        this.roundOffCurrencyFields(awbResponse);
        // Rounding off Weight fields
        this.roundOffWeightFields(awbResponse);
        this.roundOffVolumeFields(awbResponse);
        setCustomOriginCodeInResponse(awbResponse);
        return awbResponse;
    }


    private void checkAcasFlagInAwb(AwbAirMessagingResponse awbResponse) {
        awbResponse.setAcasEnabled(awbResponse.getAwbRoutingInfo().stream().map(AwbRoutingInfoResponse::getDestinationPortName)
                .anyMatch(destinationPort -> destinationPort.startsWith("US"))); // Check if starts with "US"

    }


    private void setShipperConsgineeDetailsInResponse(Awb awb, ShipmentDetails shipmentDetails, OrgAddressResponse response, AwbAirMessagingResponse awbResponse) {
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
    }

    private void setUserDataInAwbResponseMeta(ShipmentDetails shipmentDetails, AwbAirMessagingResponse awbResponse) {
        if(awbResponse.getMeta() != null) {
            var user = UserContext.getUser();

            awbResponse.getMeta().setUserInfo(populateUserInfoFields(user));
            awbResponse.getMeta().setMasterAwbNumber(shipmentDetails.getMasterBill());
            awbResponse.getMeta().setEntityNumber(shipmentDetails.getShipmentId());
        }
    }

    private void setCustomOriginCodeInResponse(AwbAirMessagingResponse awbResponse) {
        if (!Objects.isNull(awbResponse.getAwbCargoInfo()) && StringUtility.isNotEmpty(awbResponse.getAwbCargoInfo().getCustomOriginCode())) {
            String countryCode = awbResponse.getAwbCargoInfo().getCustomOriginCode();
            awbResponse.getMeta().setCustomOriginCode(!StringUtility.isNotEmpty(countryCode) && countryCode.length() == 3 ? CountryListHelper.ISO3166.fromAlpha3(awbResponse.getAwbCargoInfo().getCustomOriginCode()).getAlpha2() : awbResponse.getAwbCargoInfo().getCustomOriginCode());
        }
    }

    private void setIssuingAgentInResponse(Awb awb, OrgAddressResponse response, Parties issuingAgent, AwbAirMessagingResponse awbResponse) {
        if(response.getOrganizations().containsKey(issuingAgent.getOrgCode())
                && response.getAddresses().containsKey(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode())) {
            var org = response.getOrganizations().get(issuingAgent.getOrgCode());
            var address = response.getAddresses().get(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode());
            awbResponse.getMeta().setIssueingAgent(populateOrgsFields(org, address, awb.getAwbShipmentInfo().getIssuingAgentCountry(), awb.getAwbShipmentInfo().getIssuingAgentCity(), awb.getAwbShipmentInfo().getIssuingAgentZipCode()));
        }
    }

    private List<String> getUnlocoRequests(Awb awb, ShipmentDetails shipmentDetails) {
        List<String> unlocoRequests = new ArrayList<>();
        if(awb.getAwbNotifyPartyInfo() != null) {
            awb.getAwbNotifyPartyInfo().forEach(party -> {
                if (party.getSpecifiedAddressLocation() != null)
                    unlocoRequests.add(party.getSpecifiedAddressLocation());
            });
        }
        if(awb.getAwbOtherInfo() != null && awb.getAwbOtherInfo().getExecutedAt() != null) {
            unlocoRequests.add(awb.getAwbOtherInfo().getExecutedAt());
        }

        unlocoRequests.add(shipmentDetails.getCarrierDetails().getOriginPort());
        unlocoRequests.add(shipmentDetails.getCarrierDetails().getDestinationPort());
        return unlocoRequests;
    }

    private List<Parties> getOrgLists(ShipmentDetails shipmentDetails, Parties issuingAgent) {
        List<Parties> orgLists = new ArrayList<>();
        if(shipmentDetails.getConsigner() != null){
            orgLists.add(shipmentDetails.getConsigner());
        }
        if(shipmentDetails.getConsignee() != null){
            orgLists.add(shipmentDetails.getConsignee());
        }
        if(issuingAgent != null) {
            orgLists.add(issuingAgent);
        }
        return orgLists;
    }

    private Parties getIssuingAgent(ShipmentDetails shipmentDetails) {
        Parties issuingAgent = null;
        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                issuingAgent = orgRow;
            }
        }
        return issuingAgent;
    }

    private void processUnLocData(Awb awb, ShipmentDetails shipmentDetails, Map<String, UnlocationsResponse> unlocationsMap, AwbAirMessagingResponse awbResponse) {
        if(awb.getAwbOtherInfo() != null && unlocationsMap.containsKey(awb.getAwbOtherInfo().getExecutedAt())) {
            var unloc = unlocationsMap.get(awb.getAwbOtherInfo().getExecutedAt());
            awbResponse.getMeta().setExecutedAtCity(unloc.getNameWoDiacritics());
        }
        if(unlocationsMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            var unloc = unlocationsMap.get(shipmentDetails.getCarrierDetails().getOriginPort());
            awbResponse.getMeta().setPol(populateUnlocFields(unloc));
        }
        if(unlocationsMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            var unloc = unlocationsMap.get(shipmentDetails.getCarrierDetails().getDestinationPort());
            awbResponse.getMeta().setPod(populateUnlocFields(unloc));
        }
    }

    private void processAwbPaymentInfo(AwbAirMessagingResponse awbResponse) {
        if(awbResponse.getAwbPaymentInfo() != null) {
            if (!Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalCollect()) && !Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalPrepaid()))
                awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalCollect().max(awbResponse.getAwbPaymentInfo().getTotalPrepaid()));
            else if (!Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalCollect()))
                awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalCollect());
            else if (!Objects.isNull(awbResponse.getAwbPaymentInfo().getTotalPrepaid()))
                awbResponse.getMeta().setTotalAmount(awbResponse.getAwbPaymentInfo().getTotalPrepaid());
        }
    }

    private void processAwbRoutingInfo(AwbAirMessagingResponse awbResponse, Map<String, UnlocationsResponse> unlocationsMap, Map<String, EntityTransferCarrier> carriersMap) {
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
    }

    private void processEmptyIssuingAgent(Awb awb, TenantModel tenantModel, AwbAirMessagingResponse awbResponse) {
        var orgs = masterDataUtils.fetchOrganizations("Id", tenantModel.getDefaultOrgId());
        if(!orgs.isEmpty()) {

            CommonV1ListRequest addressRequest = new CommonV1ListRequest();
            List<Object> addressField = new ArrayList<>(List.of("Id"));
            List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", tenantModel.getDefaultAddressId()));
            addressRequest.setCriteriaRequests(addressCriteria);
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);

            String number = null;
            String expiry = null;
            Boolean isRA = false;
            Boolean isKC = false;
            if(addressList != null && !addressList.isEmpty()){
                EntityTransferAddress address = addressList.stream().findFirst().orElse(EntityTransferAddress.builder().build());
                number = address.getKCRANumber();
                expiry = address.getKCRAExpiry();
                isRA = address.getRegulatedAgent();
                isKC = address.getKnownConsignor();
            }
            awbResponse.getMeta().setIssueingAgent(AwbAirMessagingResponse.OrgDetails.builder()
                    .city(awb.getAwbShipmentInfo().getIssuingAgentCity())
                    .country(CountryListHelper.ISO3166.getAlpha2IfAlpha3(awb.getAwbShipmentInfo().getIssuingAgentCountry()))
                    .currency(orgs.get(0).getCurrencyCode())
                    .expiry(expiry != null ? LocalDateTime.parse(expiry): null)
                    .number(number)
                    .postCode(awb.getAwbShipmentInfo().getIssuingAgentZipCode())
                    .isRA(isRA).isKC(isKC)
                    .build());
        }
    }

    private AwbAirMessagingResponse.UserInfo populateUserInfoFields(UsersDto user) {
        return AwbAirMessagingResponse.UserInfo.builder()
                .userName(user.getUsername())
                .userDisplayName(user.getDisplayName())
                .build();
    }

    private void populateMasterAwbData(AwbAirMessagingResponse awbResponse, Awb masterAwb) throws RunnerException {
        BigDecimal masterGrossWeightSum = BigDecimal.ZERO;
        String masterGrossWeightSumUnit = Constants.WEIGHT_UNIT_KG;
        Integer masterPackCount = 0;

        if(masterAwb.getAwbGoodsDescriptionInfo() != null) {
            for(var good : masterAwb.getAwbGoodsDescriptionInfo()){
                if(!Objects.equals(good.getGrossWtUnit(), Constants.WEIGHT_UNIT_KG))
                    masterGrossWeightSum = masterGrossWeightSum.add(new BigDecimal(convertUnit(Constants.MASS, good.getGrossWt(), good.getGrossWtUnit(), Constants.WEIGHT_UNIT_KG).toString()));
                else
                    masterGrossWeightSum = masterGrossWeightSum.add(good.getGrossWt());
                masterPackCount += good.getPiecesNo();
            }
        }

        awbResponse.getMeta().setMasterGrossWeightSum(masterGrossWeightSum);
        awbResponse.getMeta().setMasterGrossWeightSumUnit(masterGrossWeightSumUnit);
        awbResponse.getMeta().setMasterPackCount(masterPackCount);

    }

    private void roundOffCurrencyFields(AwbAirMessagingResponse awbResponse) {
        int decimalPlaces = Optional.ofNullable(commonUtils.getCurrentTenantSettings().getCurrencyDecimalPlace()).orElse(0);

        if(awbResponse.getAwbPaymentInfo() != null) {
            if (awbResponse.getAwbPaymentInfo().getWeightCharges() != null) {
                awbResponse.getAwbPaymentInfo().setWeightCharges(awbResponse.getAwbPaymentInfo().getWeightCharges().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
            if (awbResponse.getAwbPaymentInfo().getDueAgentCharges() != null) {
                awbResponse.getAwbPaymentInfo().setDueAgentCharges(awbResponse.getAwbPaymentInfo().getDueAgentCharges().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
            if (awbResponse.getAwbPaymentInfo().getDueCarrierCharges() != null) {
                awbResponse.getAwbPaymentInfo().setDueCarrierCharges(awbResponse.getAwbPaymentInfo().getDueCarrierCharges().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
            if(awbResponse.getAwbPaymentInfo().getValuationCharge() != null) {
                awbResponse.getAwbPaymentInfo().setValuationCharge(awbResponse.getAwbPaymentInfo().getValuationCharge().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
            if(awbResponse.getAwbPaymentInfo().getTax() != null) {
                awbResponse.getAwbPaymentInfo().setTax(awbResponse.getAwbPaymentInfo().getTax().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
            if(awbResponse.getAwbPaymentInfo().getTotalPrepaid() != null) {
                awbResponse.getAwbPaymentInfo().setTotalPrepaid(awbResponse.getAwbPaymentInfo().getTotalPrepaid().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
            if(awbResponse.getAwbPaymentInfo().getTotalCollect() != null) {
                awbResponse.getAwbPaymentInfo().setTotalCollect(awbResponse.getAwbPaymentInfo().getTotalCollect().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
        }
        processAwbGoodsDescriptionInfo(awbResponse, decimalPlaces);
        processAirMessagingAdditionalFields(awbResponse, decimalPlaces);
        processAwbOtherChargesInfo(awbResponse, decimalPlaces);
        processMetaTotalAmount(awbResponse, decimalPlaces);
    }

    private void processMetaTotalAmount(AwbAirMessagingResponse awbResponse, int decimalPlaces) {
        if(awbResponse.getMeta().getTotalAmount() != null) {
            awbResponse.getMeta().setTotalAmount(awbResponse.getMeta().getTotalAmount().setScale(decimalPlaces, RoundingMode.HALF_UP));
        }
    }

    private void processAwbOtherChargesInfo(AwbAirMessagingResponse awbResponse, int decimalPlaces) {
        if(awbResponse.getAwbOtherChargesInfo() != null) {
            awbResponse.getAwbOtherChargesInfo().forEach(charge -> {
                if (charge.getAmount() != null) {
                    charge.setAmount(charge.getAmount().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
            });
        }
    }

    private void processAirMessagingAdditionalFields(AwbAirMessagingResponse awbResponse, int decimalPlaces) {
        if(awbResponse.getAirMessagingAdditionalFields() != null) {
            if(awbResponse.getAirMessagingAdditionalFields().getCcchargesInDestinationCurrency() != null)
                awbResponse.getAirMessagingAdditionalFields().setCcchargesInDestinationCurrency(awbResponse.getAirMessagingAdditionalFields().getCcchargesInDestinationCurrency().setScale(decimalPlaces, RoundingMode.HALF_UP));
            if(awbResponse.getAirMessagingAdditionalFields().getChargesAtDestination() != null) {
                awbResponse.getAirMessagingAdditionalFields().setChargesAtDestination(awbResponse.getAirMessagingAdditionalFields().getChargesAtDestination().setScale(decimalPlaces, RoundingMode.HALF_UP));
            }
        }
    }

    private void processAwbGoodsDescriptionInfo(AwbAirMessagingResponse awbResponse, int decimalPlaces) {
        if(awbResponse.getAwbGoodsDescriptionInfo() != null) {
            awbResponse.getAwbGoodsDescriptionInfo().forEach(good -> {
                if(good.getRateCharge() != null) {
                    good.setRateCharge(good.getRateCharge().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
                if(good.getTotalAmount() != null) {
                    good.setTotalAmount(good.getTotalAmount().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
            });
        }
    }

    private void roundOffWeightFields(AwbAirMessagingResponse awbResponse) {
        int decimalPlaces = Optional.ofNullable(commonUtils.getCurrentTenantSettings().getWeightDecimalPlace()).orElse(0);

        if(awbResponse.getAwbPackingInfo() != null) {
            awbResponse.getAwbPackingInfo().forEach(pack -> {
                if (pack.getWeight() != null) {
                    pack.setWeight(pack.getWeight().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
            });
        }
        if(awbResponse.getAwbGoodsDescriptionInfo() != null) {
            awbResponse.getAwbGoodsDescriptionInfo().forEach(good -> {
                if(good.getGrossWt() != null) {
                    good.setGrossWt(good.getGrossWt().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
                if(good.getChargeableWt() != null) {
                    good.setChargeableWt(good.getChargeableWt().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
                if(good.getUldTareWeight() != null) {
                    good.setUldTareWeight(good.getUldTareWeight().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
            });
        }
        if(awbResponse.getMeta().getMasterGrossWeightSum() != null) {
            awbResponse.getMeta().setMasterGrossWeightSum(awbResponse.getMeta().getMasterGrossWeightSum().setScale(decimalPlaces, RoundingMode.HALF_UP));
        }
    }

    private void roundOffVolumeFields(AwbAirMessagingResponse awbResponse) {
        int decimalPlaces = Optional.ofNullable(commonUtils.getCurrentTenantSettings().getVolumeDecimalPlace()).orElse(0);
        if(awbResponse.getAwbGoodsDescriptionInfo() != null) {
            awbResponse.getAwbGoodsDescriptionInfo().forEach(good -> {
                if(good.getGrossVolume() != null) {
                    good.setGrossVolume(good.getGrossVolume().setScale(decimalPlaces, RoundingMode.HALF_UP));
                }
            });
        }
    }

    public void createStatusUpdateForAirMessaging(AirMessagingStatusDto airMessageStatus) throws RunnerException, MessagingException, IOException {
        var guid = airMessageStatus.getGuid();
        log.info("Air-messaging : entered createStatusUpdateForAirMessaging; guid {}", guid);
        Optional<Awb> awb = Optional.ofNullable(awbDao.findAwbByGuidByQuery(guid));
        if(awb.isEmpty()){
            throw new RunnerException("No Awb exist for given Guid: " + guid);
        }
        var tenantId = awb.get().getTenantId();

        AirMessagingStatus status = getAirMessagingStatus(airMessageStatus);

        String xmlPayload = airMessageStatus.getXmlPayload() != null ? new String(Base64.decodeBase64(airMessageStatus.getXmlPayload()), StandardCharsets.UTF_8) : null;


        switch (status) {
            case FAILED -> awbDao.updateAirMessageStatus(guid, AwbStatus.AIR_MESSAGE_FAILED.name());
            case SUBMITTED -> awbDao.updateAirMessageStatus(guid, AwbStatus.AIR_MESSAGE_SENT.name());
            case SUCCESS -> awbDao.updateAirMessageStatus(guid, AwbStatus.AIR_MESSAGE_SUCCESS.name());
            default -> log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, status);
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

            processAwbConsolidationId(guid, status, masterAwb);


        } else if (awb.get().getShipmentId() != null) {
            var shipmentDetailsList = shipmentDao.getShipmentNumberFromId(List.of(awb.get().getShipmentId()));
            String msgType = getMsgType(airMessageStatus, shipmentDetailsList);
            airMessagingLogsDao.createAirMessagingLogs(UUID.randomUUID(), guid, airMessageStatus.getErrorMessage(),
                    msgType, xmlPayload, status.name(), tenantId, LocalDateTime.now());
            eventDao.createEventForAirMessagingStatus(UUID.randomUUID(), awb.get().getShipmentId(),
                    Constants.SHIPMENT, "FNM", "FNM received", LocalDateTime.now(), LocalDateTime.now(),
                    Constants.DESCARTES, tenantId, status.name(), LocalDateTime.now(), LocalDateTime.now());

            processAwbShipmentId(guid, masterAwb, awb);

        }
    }

    private String getMsgType(AirMessagingStatusDto airMessageStatus, List<ShipmentDetails> shipmentDetailsList) {
        String msgType = airMessageStatus.getMessageType();
        if(shipmentDetailsList != null && !shipmentDetailsList.isEmpty() && msgType == null){
            if(Objects.equals(shipmentDetailsList.get(0).getJobType(), Constants.SHIPMENT_TYPE_DRT))
                msgType = "FWB";
            else
                msgType = "FZB";
        }
        return msgType;
    }

    private void processAwbShipmentId(UUID guid, Awb masterAwb, Optional<Awb> awb) {
        List<Awb> awbsList = awbDao.findAllLinkedAwbs(guid);

        AwbStatus hawbsStatus = null;
        UUID consoleGuid = null;
        boolean allStatusReceived = true;
        AwbStatus consoleStatus = null;

        if(awbsList != null && !awbsList.isEmpty()){
            for (var x: awbsList) {
                if(x.getShipmentId() != null){
                    hawbsStatus = getHawbsStatusForShipmentId(x, hawbsStatus);
                } else if (x.getConsolidationId() != null) {
                    consoleGuid = x.getGuid();
                    consoleStatus = x.getAirMessageStatus();
                    masterAwb = x;
                }
                allStatusReceived = isAllStatusReceived(x, allStatusReceived);
            }
            if(awbsList.size() == 1){
                masterAwb = awbsList.get(0);
            }
        }
        if(masterAwb == null && awb.isPresent()) {
            consoleStatus = awb.get().getAirMessageStatus();
            masterAwb = awb.get();
        }
        if(hawbsStatus != null && consoleGuid != null)
            awbDao.updateLinkedHawbAirMessageStatus(consoleGuid, hawbsStatus.name());

        sendFailureEmails(masterAwb, allStatusReceived, hawbsStatus, consoleStatus, awbsList);
    }

    private void sendFailureEmails(Awb masterAwb, boolean allStatusReceived, AwbStatus hawbsStatus, AwbStatus consoleStatus, List<Awb> awbsList) {
        if(Boolean.TRUE.equals(allStatusReceived) && (Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED) || Objects.equals(consoleStatus, AwbStatus.AIR_MESSAGE_FAILED))) {
            try {
                this.sendAirMessagingFailureEmail(masterAwb, awbsList);
            } catch (Exception e) {
                log.error(Constants.SEND_EMAIL_AIR_MESSAGING_FAILURE, e.getMessage());
            }
        }
    }

    private boolean isAllStatusReceived(Awb x, boolean allStatusReceived) {
        if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
            allStatusReceived = false;
        }
        return allStatusReceived;
    }

    private AwbStatus getHawbsStatusForShipmentId(Awb x, AwbStatus hawbsStatus) {
        if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
            hawbsStatus = AwbStatus.AIR_MESSAGE_SENT;
        } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_FAILED) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT)) {
            hawbsStatus = AwbStatus.AIR_MESSAGE_FAILED;
        } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SUCCESS) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED)) {
            hawbsStatus = AwbStatus.AIR_MESSAGE_SUCCESS;
        }
        return hawbsStatus;
    }

    private void processAwbConsolidationId(UUID guid, AirMessagingStatus status, Awb masterAwb) {
        List<Awb> awbsList = awbDao.findAllLinkedAwbs(guid);

        AwbStatus hawbsStatus = null;
        boolean allStatusReceived = true;
        if(awbsList != null && !awbsList.isEmpty()){
            for (var x: awbsList) {
                hawbsStatus = getHawbsStatus(x, hawbsStatus);
                allStatusReceived = isAllStatusReceived(x, allStatusReceived);
            }
        }
        if(hawbsStatus != null)
            awbDao.updateLinkedHawbAirMessageStatus(guid, hawbsStatus.name());

        if(Boolean.TRUE.equals(allStatusReceived) && (Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED) || Objects.equals(status, AwbStatus.AIR_MESSAGE_FAILED))) {
            try {
                this.sendAirMessagingFailureEmail(masterAwb, awbsList);
            } catch (Exception e) {
                log.error(Constants.SEND_EMAIL_AIR_MESSAGING_FAILURE, e.getMessage());
            }
        }
    }

    private AwbStatus getHawbsStatus(Awb x, AwbStatus hawbsStatus) {
        if(x.getShipmentId() != null){
            if(Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT)) {
                hawbsStatus = AwbStatus.AIR_MESSAGE_SENT;
            } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_FAILED) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT)) {
                hawbsStatus = AwbStatus.AIR_MESSAGE_FAILED;
            } else if (Objects.equals(x.getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SUCCESS) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_SENT) && !Objects.equals(hawbsStatus, AwbStatus.AIR_MESSAGE_FAILED)) {
                hawbsStatus = AwbStatus.AIR_MESSAGE_SUCCESS;
            }
        }
        return hawbsStatus;
    }

    private AirMessagingStatus getAirMessagingStatus(AirMessagingStatusDto airMessageStatus) throws RunnerException {
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
        return status;
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
                var shipmentList = shipmentDao.findShipmentsByIds(consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).collect(Collectors.toSet()));
                shipmentList.forEach(shipment -> eventDao.createEventForAirMessagingEvent(prepareEventPayload(airMessageEvent, shipment.getId(), Constants.SHIPMENT, shipment.getTenantId())));
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
                body = getBodyForShipmentIds(awbsList, shipmentIds, body);
            }
        }
        body = body + "\n \n Please rectify the data as per the failure comment and resend the messages by clicking on the \"Print\" button in the MAWB. \n \n" +
                        "Thank you!\n" + "Cargoes Runner";
        emailServiceUtility.sendEmail(body, subject, emailIds, null, null, null);
    }

    private String getBodyForShipmentIds(List<Awb> awbsList, List<Long> shipmentIds, String body) {
        StringBuilder bodyStringBuilder = new StringBuilder(body);
        var shipmentDetailsList = shipmentDao.getShipmentNumberFromId(shipmentIds);
        Map<Long, String> map = shipmentDetailsList.stream().collect(Collectors.toMap(ShipmentDetails::getId, ShipmentDetails::getShipmentId));
        for (var x : awbsList) {
            if (x.getShipmentId() != null) {
                String shipNumber = map.get(x.getShipmentId());
                AirMessagingLogs shipAirMessagingLogs = airMessagingLogsService.getRecentLogForEntityGuid(x.getGuid());
                if (shipAirMessagingLogs != null) {
                    if (Objects.equals(shipAirMessagingLogs.getStatus(), AirMessagingStatus.SUCCESS.name())) {
                        bodyStringBuilder = new StringBuilder(body + "FZB for \"" + shipNumber + "\" : Success\n");
                    } else if (Objects.equals(shipAirMessagingLogs.getStatus(), AirMessagingStatus.FAILED.name())) {
                        bodyStringBuilder = new StringBuilder(body + "FZB for \"" + shipNumber + "\" : Failed. Failure reason is \"" + shipAirMessagingLogs.getErrorMessage() + "\"\n\n");
                    }
                }
            }
        }
        return bodyStringBuilder.toString();
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
