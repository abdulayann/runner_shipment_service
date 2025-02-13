package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.DeliveryOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getCityCountry;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

@Component
public class DeliveryOrderReport extends IReport {

    public Boolean printWithoutTranslation;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private CommonUtils commonUtils;

    @Override
    public Map<String, Object> getData(Long id) {
        DeliveryOrderModel deliveryOrderModel = (DeliveryOrderModel) getDocumentModel(id);
        return populateDictionary(deliveryOrderModel);
    }


    public Map<String, Object> getData(Long id, Long transportInstructionId) {
        DeliveryOrderModel deliveryOrderModel = (DeliveryOrderModel) getDocumentModel(id);
        deliveryOrderModel.setTransportInstructionId(transportInstructionId);
        return populateDictionary(deliveryOrderModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        DeliveryOrderModel deliveryOrderModel = new DeliveryOrderModel();
        deliveryOrderModel.shipmentDetails = getShipment(id);
        validateAirDGCheckShipments(deliveryOrderModel.shipmentDetails);
        validateAirAndOceanDGCheck(deliveryOrderModel.shipmentDetails);
        deliveryOrderModel.usersDto = UserContext.getUser();
        deliveryOrderModel.shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if (deliveryOrderModel.shipmentDetails.getConsolidationList() != null && deliveryOrderModel.shipmentDetails.getConsolidationList().size() > 0) {
            deliveryOrderModel.consolidationDetails = deliveryOrderModel.shipmentDetails.getConsolidationList().get(0);
            UnlocationsResponse placeOfIssue = null;
            if (StringUtility.isNotEmpty(deliveryOrderModel.consolidationDetails.getPlaceOfIssue())) {
                List<Object> criteria = Arrays.asList(
                        List.of(EntityTransferConstants.LOCATION_SERVICE_GUID),
                        "=",
                        deliveryOrderModel.consolidationDetails.getPlaceOfIssue()
                );
                CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
                V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
                List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
                if (unlocationsResponse.size() > 0)
                    placeOfIssue = unlocationsResponse.get(0);
                if (placeOfIssue != null) {
                    deliveryOrderModel.placeOfIssueName = placeOfIssue.getNameWoDiacritics();
                }
            }
        }
        deliveryOrderModel.setContainers(new ArrayList<>());
        if (deliveryOrderModel.shipmentDetails.getContainersList() != null) {
            for (ContainerModel container : deliveryOrderModel.shipmentDetails.getContainersList())
                deliveryOrderModel.getContainers().add(getShipmentContainer(container));
        }
        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, deliveryOrderModel.shipmentDetails.getPaymentTerms());
        deliveryOrderModel.paymentTerms = (masterData != null ? masterData.getItemDescription() : null);
        deliveryOrderModel.hbl = getHbl(id);
        deliveryOrderModel.tenantModel = getTenant();
        return deliveryOrderModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        DeliveryOrderModel deliveryOrderModel = (DeliveryOrderModel) documentModel;
        List<String> orgWithoutTranslation = new ArrayList<>();
        List<String> chargeTypesWithoutTranslation = new ArrayList<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(deliveryOrderModel.shipmentDetails, GetDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        deliveryOrderModel.shipmentDetails.setTransportInstructionId(deliveryOrderModel.getTransportInstructionId());
        populateShipmentFields(deliveryOrderModel.shipmentDetails, dictionary);
        populateConsolidationFields(deliveryOrderModel.consolidationDetails, dictionary);
        populateUserFields(deliveryOrderModel.usersDto, dictionary);
        populateTenantFields(dictionary, deliveryOrderModel.getTenantModel());
        populateBlFields(deliveryOrderModel.hbl, dictionary);
        populateBillChargesFields(deliveryOrderModel.shipmentDetails, dictionary);
        populateShipmentOrganizationsLL(deliveryOrderModel.shipmentDetails, dictionary, orgWithoutTranslation);
        dictionary.put(ReportConstants.MASTER_BILL_ISSUE_PLACE, deliveryOrderModel.placeOfIssueName);
        dictionary.put(ReportConstants.PPCC, deliveryOrderModel.paymentTerms);

        if (deliveryOrderModel.shipmentDetails.getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(deliveryOrderModel.shipmentDetails.getAdditionalDetails().getNotifyParty()));
        }
        dictionary.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(deliveryOrderModel.shipmentDetails.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.VOLUME, ConvertToVolumeNumberFormat(deliveryOrderModel.shipmentDetails.getVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CHARGEABLE, ConvertToWeightNumberFormat(deliveryOrderModel.shipmentDetails.getChargable(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.NetWeight, ConvertToWeightNumberFormat(deliveryOrderModel.shipmentDetails.getNetWeight(), v1TenantSettingsResponse));
        if (deliveryOrderModel.getContainers() != null && deliveryOrderModel.getContainers().size() > 0) {
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ShipmentContainers shipmentContainers : deliveryOrderModel.getContainers()) {
                valuesContainer.add(jsonHelper.convertValue(shipmentContainers, new TypeReference<>() {
                }));
            }
            for (Map<String, Object> v : valuesContainer) {
                if (v.containsKey(ReportConstants.GROSS_VOLUME) && v.get(ReportConstants.GROSS_VOLUME) != null)
                    v.put(ReportConstants.GROSS_VOLUME, ConvertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.GROSS_WEIGHT) && v.get(ReportConstants.GROSS_WEIGHT) != null) {
                    String grossWeight = ConvertToWeightNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse);
                    v.put(ReportConstants.GROSS_WEIGHT, grossWeight);
                    v.put(CARGO_GROSS_WEIGHT_UNIT, grossWeight + " " + v.get(GROSS_WEIGHT_UNIT));
                }

                if (v.containsKey(ReportConstants.NetWeight) && v.get(ReportConstants.NetWeight) != null)
                    v.put(ReportConstants.NetWeight, ConvertToWeightNumberFormat(v.get(ReportConstants.NetWeight), v1TenantSettingsResponse));
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        }

        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(deliveryOrderModel.getContainers()));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, deliveryOrderModel.getContainers());

        //Add P0 tags
        PickupDeliveryDetailsModel deliveryDetails = deliveryOrderModel.shipmentDetails.getDeliveryDetails();
        if (deliveryDetails != null) {
            LocalDateTime deliveryTime = deliveryDetails.getActualPickupOrDelivery() != null ? deliveryDetails.getActualPickupOrDelivery() :
                    deliveryDetails.getEstimatedPickupOrDelivery();
            dictionary.put(DELIVERY_TIME, convertToDPWDateFormatWithTime(deliveryTime, v1TenantSettingsResponse.getDPWDateFormat(), true));
        }

        if (!Objects.isNull(deliveryOrderModel.shipmentDetails.getPackingList()) && !deliveryOrderModel.shipmentDetails.getPackingList().isEmpty()) {
            getPackingDetails(deliveryOrderModel.shipmentDetails, dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = deliveryOrderModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = deliveryOrderModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
            if (hazardousCheck)
                dictionary.put(HAS_DANGEROUS_GOODS, true);
            else
                dictionary.put(HAS_DANGEROUS_GOODS, false);
            if (temperatureCheck)
                dictionary.put(HAS_TEMPERATURE_DETAILS, true);
            else
                dictionary.put(HAS_TEMPERATURE_DETAILS, false);

        } else {
            dictionary.put(HAS_PACK_DETAILS, false);
        }

//        getPackingDetails(deliveryOrderModel.shipmentDetails, dictionary);

        if (dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List) {
            List<Map<String, Object>> values = (List<Map<String, Object>>) dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> v : values) {
                if (v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String) v.get(CHARGE_TYPE_CODE), chargeTypesWithoutTranslation));
                }
            }
        }

        PartiesModel deliveryTo = null;

        if (deliveryOrderModel.shipmentDetails.getDeliveryDetails() != null)
            deliveryTo = deliveryOrderModel.shipmentDetails.getDeliveryDetails().getDestinationDetail();
        if (deliveryTo != null && deliveryTo.getAddressData() != null) {
            Map<String, Object> addressMap = deliveryTo.getAddressData();
            populateAddress(addressMap, dictionary, ReportConstants.DeliveryTo);
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put(ReportConstants.DeliveryTo, address);
        }
        Integer decimalPlaces = commonUtils.getShipmentSettingFromContext().getDecimalPlaces() == null ? 2 : commonUtils.getShipmentSettingFromContext().getDecimalPlaces();
        if (!Objects.isNull(deliveryOrderModel.shipmentDetails.getWeight())) {
            BigDecimal weight = deliveryOrderModel.shipmentDetails.getWeight().setScale(decimalPlaces, RoundingMode.HALF_UP);
            String weightString = ConvertToWeightNumberFormat(weight, v1TenantSettingsResponse);
            dictionary.put(WEIGHT, weightString);
            dictionary.put(WEIGHT_AND_UNIT, String.format(REGEX_S_S, weightString, deliveryOrderModel.shipmentDetails.getWeightUnit()));
        }
        if (!Objects.isNull(deliveryOrderModel.shipmentDetails.getVolume())) {
            BigDecimal volume = deliveryOrderModel.shipmentDetails.getVolume().setScale(decimalPlaces, RoundingMode.HALF_UP);
            String volumeString = ConvertToVolumeNumberFormat(volume, v1TenantSettingsResponse);
            dictionary.put(VOLUME, volumeString);
            dictionary.put(VOLUME_AND_UNIT, String.format(REGEX_S_S, volumeString, deliveryOrderModel.shipmentDetails.getVolumeUnit()));
        }
        if (!Objects.isNull(deliveryOrderModel.shipmentDetails.getChargable())) {
            BigDecimal chargeable = deliveryOrderModel.shipmentDetails.getChargable().setScale(decimalPlaces, RoundingMode.HALF_UP);
            String chargeableString = ConvertToWeightNumberFormat(chargeable, v1TenantSettingsResponse);
            dictionary.put(CHARGEABLE, chargeableString);
            dictionary.put(CHARGEABLE_AND_UNIT, String.format(REGEX_S_S, chargeableString, deliveryOrderModel.shipmentDetails.getChargeableUnit()));
            dictionary.put(CHARGEABLE_AND_UNIT_, dictionary.get(CHARGEABLE_AND_UNIT));
        }
        PartiesModel client = deliveryOrderModel.shipmentDetails.getClient();
        if (client != null && client.getAddressData() != null) {
            Map<String, Object> addressMap = client.getAddressData();
            List<String> clientAddress = getOrgAddress(getValueFromMap(addressMap, COMPANY_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    null, null);
            if (!Boolean.TRUE.equals(deliveryOrderModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(addressMap, FULL_NAME) != null) {
                clientAddress.add(0, getValueFromMap(addressMap, FULL_NAME));
            }
            dictionary.put(CLIENT_ADRS, clientAddress);
        }

        if (!Objects.isNull(deliveryOrderModel.consolidationDetails) && !Objects.isNull(deliveryOrderModel.consolidationDetails.getArrivalDetails())) {
            if (deliveryOrderModel.consolidationDetails.getArrivalDetails().getCTOId() != null)
                dictionary.put(CTO_FULL_NAME, getValueFromMap(deliveryOrderModel.consolidationDetails.getArrivalDetails().getCTOId().getOrgData(), FULL_NAME));
        }

        populateRaKcData(dictionary, deliveryOrderModel.getShipmentDetails());
        HandleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        return dictionary;
    }
}
