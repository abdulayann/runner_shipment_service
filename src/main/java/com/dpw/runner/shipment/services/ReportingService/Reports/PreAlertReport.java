package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.PreAlertModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIRLINE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.JOB_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NOTIFY_PARTY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

@Component
@Slf4j
public class PreAlertReport extends IReport {

    public static final String COMMODITY = "Commodity";
    @Autowired
    private JsonHelper jsonHelper;

    public Boolean printWithoutTranslation;

    @Autowired
    private CommonUtils commonUtils;

    @Override
    public Map<String, Object> getData(Long id) {
        PreAlertModel preAlertModel = (PreAlertModel) getDocumentModel(id);
        return populateDictionary(preAlertModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        PreAlertModel preAlertModel = new PreAlertModel();
        preAlertModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(preAlertModel.shipmentDetails);
        preAlertModel.tenantDetails = getTenant();
        preAlertModel.shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        preAlertModel.consolidationDetails = getFirstConsolidationFromShipmentId(id);
        if(preAlertModel.shipmentDetails.getContainersList() != null && !preAlertModel.shipmentDetails.getContainersList().isEmpty()) {
            List<ShipmentContainers> shipmentContainersList = new ArrayList<>();
            for (ContainerModel containerModel: preAlertModel.shipmentDetails.getContainersList()) {
                ShipmentContainers shipmentContainers = getShipmentContainer(containerModel);
                shipmentContainersList.add(shipmentContainers);
            }
            if(!shipmentContainersList.isEmpty())
                preAlertModel.setShipmentContainers(shipmentContainersList);
        }
        preAlertModel.shipmentDetails.setShipmentContainersList(preAlertModel.getShipmentContainers());
        preAlertModel.noofpackages_word = numberToWords(preAlertModel.shipmentDetails.getNoOfPacks());
        preAlertModel.userdisplayname = UserContext.getUser().DisplayName;
        return preAlertModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        PreAlertModel preAlertModel = (PreAlertModel) documentModel;
        List<String> orgWithoutTranslation = new ArrayList<>();
        List<String> chargeTypesWithoutTranslation = new ArrayList<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(preAlertModel.shipmentDetails, GetDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        JsonDateFormat(dictionary);
        addTenantDetails(dictionary, preAlertModel.tenantDetails);
        populateShipmentFields(preAlertModel.shipmentDetails, dictionary);
        populateShipmentOrganizationsLL(preAlertModel.shipmentDetails, dictionary, orgWithoutTranslation);
        List<String> consigner = getConsigner(preAlertModel, dictionary);
        List<String> consignee = getConsignee(preAlertModel, dictionary);
        List<String> notify = new ArrayList<>();
        if (preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null) {
            notify = getOrgAddressWithPhoneEmail(preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty());
        }
        if (preAlertModel.shipmentDetails.getClient() != null && preAlertModel.shipmentDetails.getClient().getAddressData() != null && getValueFromMap(preAlertModel.shipmentDetails.getClient().getAddressData(), ReportConstants.COMPANY_NAME) != null) {
            dictionary.put(ReportConstants.CLIENT_NAME, getValueFromMap(preAlertModel.shipmentDetails.getClient().getAddressData(), ReportConstants.COMPANY_NAME));
        }
        processConsginerDetails(preAlertModel, dictionary, consigner);
        processConsigneeDetails(preAlertModel, dictionary, consignee);
        processNotifyParty(preAlertModel, dictionary, notify);
        if(preAlertModel.shipmentDetails.getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty()));
        }

        addPartyAddressTags(dictionary, preAlertModel);
        List<String> tenantsDataList = getListOfStrings(preAlertModel.tenantDetails.tenantName, preAlertModel.tenantDetails.address1, preAlertModel.tenantDetails.address2,
                preAlertModel.tenantDetails.city, preAlertModel.tenantDetails.state, preAlertModel.tenantDetails.zipPostCode, preAlertModel.tenantDetails.country,
                preAlertModel.tenantDetails.email, preAlertModel.tenantDetails.websiteUrl, preAlertModel.tenantDetails.phone);
        dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(ReportConstants.NO_OF_PACKAGES_WORD, preAlertModel.noofpackages_word);
        dictionary.put(ReportConstants.USER_DISPLAY_NAME, preAlertModel.userdisplayname);
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.DELIVERY_AGENT, null);
        dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        addDeliveryAgentTag(preAlertModel, dictionary);
        if (preAlertModel.shipmentDetails.getCarrierDetails() != null) {
            dictionary.put(ReportConstants.ETD, ConvertToDPWDateFormat(preAlertModel.shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
        if (preAlertModel.shipmentDetails.getCarrierDetails() != null) {
            dictionary.put(ReportConstants.ETA, ConvertToDPWDateFormat(preAlertModel.shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, preAlertModel.getShipmentContainers());
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(preAlertModel.getShipmentContainers()));
        addOriginDestinationTag(preAlertModel, dictionary);
        UnlocationsResponse destination;
        UnlocationsResponse origin;
        dictionary.put(JOB_NO, preAlertModel.shipmentDetails.getShipmentId());
        dictionary.put(AIRLINE, preAlertModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, preAlertModel.shipmentDetails.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.FLIGHT_NAME, preAlertModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.MARKS_NO, preAlertModel.shipmentDetails.getMarksNum());
//        dictionary.put(ReportConstants.CMS_REMARKS, preAlertModel.shipmentDetails.) LATER- Where is Remarks Field in Shipment?
        processWeightVolumeTags(preAlertModel, dictionary, v1TenantSettingsResponse);
        dictionary.put(ReportConstants.TOTAL_PCS, preAlertModel.noofpackages_word);
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(preAlertModel.shipmentDetails);
        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        UnlocationsResponse pol = unlocationsMap.get(preAlertModel.shipmentDetails.getCarrierDetails().getOriginPort());
        UnlocationsResponse pod = unlocationsMap.get(preAlertModel.shipmentDetails.getCarrierDetails().getDestinationPort());
        origin = unlocationsMap.get(preAlertModel.shipmentDetails.getCarrierDetails().getOrigin());
        destination = unlocationsMap.get(preAlertModel.shipmentDetails.getCarrierDetails().getDestination());
        if (pol != null) {
            dictionary.put(ReportConstants.PORT_OF_DEPARTURE, pol.getPortName());
            dictionary.put(ReportConstants.PORT_OF_DEPARTURE_COUNTRY, pol.getCountry());
            dictionary.put(ReportConstants.POL_PORTNAME, pol.getPortName());
            dictionary.put(ReportConstants.POL_COUNTRY, pol.getCountry());
        }
        if (pod != null) {
            dictionary.put(ReportConstants.PORT_OF_ARRIVAL, pod.getPortName());
            dictionary.put(ReportConstants.PORT_OF_ARRIVAL_COUNTRY, pod.getCountry());
            dictionary.put(ReportConstants.POD_PORTNAME, pod.getPortName());
            dictionary.put(ReportConstants.POD_COUNTRY, pod.getCountry());
        }
        if (origin != null)
            dictionary.put(ReportConstants.PLACE_OF_RECEIPT_ALIAS, origin.getName());
        if (destination != null)
            dictionary.put(ReportConstants.PLACE_OF_DELIVERY_ALIAS, destination.getName());
        if (preAlertModel.consolidationDetails != null && preAlertModel.consolidationDetails.getPayment() != null)
            dictionary.put(ReportConstants.PPCC, preAlertModel.consolidationDetails.getPayment());
        else
            dictionary.put(ReportConstants.PPCC, null);
        processShipmentPackingList(preAlertModel, dictionary);
        populateHasContainerFields(preAlertModel.shipmentDetails, dictionary, v1TenantSettingsResponse);
        HandleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        return dictionary;
    }

    private void processWeightVolumeTags(PreAlertModel preAlertModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(preAlertModel.shipmentDetails.getVolumetricWeight() != null)
            dictionary.put(ReportConstants.V_WEIGHT_AND_UNIT, String.format(REGEX_S_S, twoDecimalPlacesFormatDecimal(preAlertModel.shipmentDetails.getVolumetricWeight()), preAlertModel.shipmentDetails.getVolumetricWeightUnit()));
        if(preAlertModel.shipmentDetails.getWeight() != null)
            dictionary.put(ReportConstants.WEIGHT_AND_UNIT, String.format(REGEX_S_S, ConvertToWeightNumberFormat(preAlertModel.shipmentDetails.getWeight(), v1TenantSettingsResponse), preAlertModel.shipmentDetails.getWeightUnit()));
        if(preAlertModel.shipmentDetails.getVolume() != null)
            dictionary.put(ReportConstants.VOLUME_AND_UNIT, String.format(REGEX_S_S, ConvertToVolumeNumberFormat(preAlertModel.shipmentDetails.getVolume(), v1TenantSettingsResponse), preAlertModel.shipmentDetails.getVolumeUnit()));
        if(preAlertModel.shipmentDetails.getVolume() != null)
            dictionary.put(ReportConstants.TOTAL_VOLUME, String.format(REGEX_S_S, ConvertToVolumeNumberFormat(preAlertModel.shipmentDetails.getVolume(), v1TenantSettingsResponse), preAlertModel.shipmentDetails.getVolumeUnit()));
        if(preAlertModel.shipmentDetails.getWeight() != null)
            dictionary.put(ReportConstants.TOTAL_WEIGHT, String.format(REGEX_S_S, ConvertToWeightNumberFormat(preAlertModel.shipmentDetails.getWeight(), v1TenantSettingsResponse), preAlertModel.shipmentDetails.getWeightUnit()));
    }

    private void addOriginDestinationTag(PreAlertModel preAlertModel, Map<String, Object> dictionary) {
        UnlocationsResponse origin = getUNLocRow(preAlertModel.shipmentDetails.getCarrierDetails().getOrigin());
        if (origin != null && origin.getIataCode() != null)
            dictionary.put(ReportConstants.ORIGIN, origin.getIataCode());
        else
            dictionary.put(ReportConstants.ORIGIN, null);
        UnlocationsResponse destination = getUNLocRow(preAlertModel.shipmentDetails.getCarrierDetails().getDestination());
        if (destination != null && destination.getIataCode() != null)
            dictionary.put(ReportConstants.DESTINATION, destination.getIataCode());
        else
            dictionary.put(ReportConstants.DESTINATION, null);
    }

    private void addDeliveryAgentTag(PreAlertModel preAlertModel, Map<String, Object> dictionary) {
        List<String> deliveryAgent;
        if (preAlertModel.shipmentDetails.getDeliveryDetails() != null && preAlertModel.shipmentDetails.getDeliveryDetails().getAgentDetail() != null) {
            deliveryAgent = getOrgAddressWithPhoneEmail(preAlertModel.shipmentDetails.getDeliveryDetails().getAgentDetail());
            if (preAlertModel.shipmentDetails.getDeliveryDetails().getAgentDetail().getOrgData() != null) {
                Map<String, Object> partyOrg = preAlertModel.shipmentDetails.getDeliveryDetails().getAgentDetail().getOrgData();
                if (!Boolean.TRUE.equals(preAlertModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    deliveryAgent.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
            dictionary.put(ReportConstants.DELIVERY_AGENT, deliveryAgent);
        }
    }

    private void addPartyAddressTags(Map<String, Object> dictionary, PreAlertModel preAlertModel) {
        dictionary.put(ReportConstants.CONSIGNER_ADDRESS, getAddressList(ReportHelper.getValueFromMap(preAlertModel.shipmentDetails.getConsigner() != null ?
                preAlertModel.shipmentDetails.getConsigner().getAddressData() : null, ReportConstants.ADDRESS1)));
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, getAddressList(ReportHelper.getValueFromMap(preAlertModel.shipmentDetails.getConsignee() != null ?
                preAlertModel.shipmentDetails.getConsignee().getAddressData() : null, ReportConstants.ADDRESS1)));
        dictionary.put(ReportConstants.NOTIFY_PARTY_ADDRESS, getAddressList(ReportHelper.getValueFromMap(preAlertModel.shipmentDetails.getAdditionalDetails() != null
                && preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null ?
                preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressData() : null, ReportConstants.ADDRESS1)));
    }

    private void processNotifyParty(PreAlertModel preAlertModel, Map<String, Object> dictionary, List<String> notify) {
        if (preAlertModel.shipmentDetails.getAdditionalDetails() != null &&
                preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null &&
                preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData() != null &&
                getValueFromMap(preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData(), ReportConstants.FULL_NAME) != null) {
            String notifyFullName = getValueFromMap(preAlertModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData(), ReportConstants.FULL_NAME);
            if(Boolean.TRUE.equals(preAlertModel.shipmentSettingsDetails.getDisableBlPartiesName())) {
                dictionary.put(ReportConstants.NOTIFY_PARTY_AIR, notify);
            } else {
                dictionary.put(ReportConstants.NOTIFY_PARTY_AIR, getCompleteNameAndAddress(notifyFullName, notify));
            }
        }
    }

    private void processConsigneeDetails(PreAlertModel preAlertModel, Map<String, Object> dictionary, List<String> consignee) {
        if (preAlertModel.shipmentDetails.getConsignee() != null &&
                preAlertModel.shipmentDetails.getConsignee().getOrgData() != null &&
                getValueFromMap(preAlertModel.shipmentDetails.getConsignee().getOrgData(), ReportConstants.FULL_NAME) != null) {
            String consigneeFullName = getValueFromMap(preAlertModel.shipmentDetails.getConsignee().getOrgData(), ReportConstants.FULL_NAME);
            dictionary.put(ReportConstants.CONSIGNEE, consigneeFullName);
            if(Boolean.TRUE.equals(preAlertModel.shipmentSettingsDetails.getDisableBlPartiesName())) {
                dictionary.put(ReportConstants.CONSIGNEE_AIR, consignee);
            } else {
                dictionary.put(ReportConstants.CONSIGNEE_AIR, getCompleteNameAndAddress(consigneeFullName, consignee));
            }
            try {
                dictionary.put(ReportConstants.CONSIGNEE_FULL_NAME, consigneeFullName);
            } catch (Exception ignored) {
                log.info(Constants.IGNORED_ERROR_MSG);
            }
        }
    }

    private void processConsginerDetails(PreAlertModel preAlertModel, Map<String, Object> dictionary, List<String> consigner) {
        if (preAlertModel.shipmentDetails.getConsigner() != null &&
                preAlertModel.shipmentDetails.getConsigner().getOrgData() != null &&
                getValueFromMap(preAlertModel.shipmentDetails.getConsigner().getOrgData(), ReportConstants.FULL_NAME) != null) {
            String consignerFullName = getValueFromMap(preAlertModel.shipmentDetails.getConsigner().getOrgData(), ReportConstants.FULL_NAME);
            dictionary.put(ReportConstants.CONSIGNER, consignerFullName);
            if(Boolean.TRUE.equals(preAlertModel.shipmentSettingsDetails.getDisableBlPartiesName())) {
                dictionary.put(ReportConstants.CONSIGNER_AIR, consigner);
            } else {
                dictionary.put(ReportConstants.CONSIGNER_AIR, getCompleteNameAndAddress(consignerFullName, consigner));
            }
            try {
                dictionary.put(ReportConstants.CONSIGNER_FULL_NAME, consignerFullName);
            } catch (Exception ignored) {
                log.info(Constants.IGNORED_ERROR_MSG);
            }
        }
    }

    private List<String> getConsignee(PreAlertModel preAlertModel, Map<String, Object> dictionary) {
        List<String> consignee = new ArrayList<>();
        if(preAlertModel.shipmentDetails.getConsignee() != null) {
            consignee = getOrgAddressWithPhoneEmail(preAlertModel.shipmentDetails.getConsignee());
            if(preAlertModel.shipmentDetails.getConsignee().getAddressData() != null) {
                dictionary.put(ReportConstants.CONSIGNEE_PHONE, preAlertModel.shipmentDetails.getConsignee().getAddressData().get("ContactPhone"));
            }
        }
        return consignee;
    }

    private List<String> getConsigner(PreAlertModel preAlertModel, Map<String, Object> dictionary) {
        List<String> consigner = new ArrayList<>();
        if(preAlertModel.shipmentDetails.getConsigner() != null) {
            consigner = getOrgAddressWithPhoneEmail(preAlertModel.shipmentDetails.getConsigner());
            if(preAlertModel.shipmentDetails.getConsigner().getAddressData() != null) {
                dictionary.put(ReportConstants.CONSIGNER_PHONE, preAlertModel.shipmentDetails.getConsigner().getAddressData().get("ContactPhone"));
            }
        }
        return consigner;
    }

    private void processShipmentPackingList(PreAlertModel preAlertModel, Map<String, Object> dictionary) {
        if(preAlertModel.shipmentDetails.getPackingList() != null && !preAlertModel.shipmentDetails.getPackingList().isEmpty()) {
            List<Map<String, Object>> packDictionary = new ArrayList<>();
            for (PackingModel pack : preAlertModel.shipmentDetails.getPackingList()) {
                String packJson = jsonHelper.convertToJson(pack);
                packDictionary.add(jsonHelper.convertJsonToMap(packJson));
            }
            if(!packDictionary.isEmpty()) {
                for(Map<String, Object> v: packDictionary) {
                    JsonDateFormat(v);
                    processCommodity(dictionary, v);
                    //
                }
            }
            dictionary.put(ReportConstants.PACKS_DETAILS, packDictionary);
            getPackingDetails(preAlertModel.shipmentDetails, dictionary);
        }
    }

    private void processCommodity(Map<String, Object> dictionary, Map<String, Object> v) {
        if(v.containsKey(COMMODITY) && v.get(COMMODITY) != null) {
            CommodityResponse commodityResponse = getCommodity(v.get(COMMODITY).toString());
            if(commodityResponse != null)
                dictionary.put(ReportConstants.COMMODITY_DESC, commodityResponse.getDescription());
        }
    }
}
