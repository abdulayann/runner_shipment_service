package com.dpw.runner.shipment.services.reportingservice.Reports;

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.AGENT_REFERENCE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CARRIER;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CARRIER_BOOKING_REF;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CHARGEABLE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CHARGEABLE_AND_UNIT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CHARGEABLE_AND_UNIT1;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CHARGEABLE_WEIGHT_DECIMAL_PLACES;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.COMMON_CONTAINERS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CONSOLIDATION_NUMBER;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CONTAINER_TYPE_CODE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CONTAINER_TYPE_DESCRIPTION;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CTO_ADDRESS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CUSTOMS_ENTRY_NUMBER;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.ETA_FOR_PRINT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.ETD_FOR_PRINT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.HAS_DANGEROUS_GOODS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.HAS_PACK_DETAILS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.HAS_TEMPERATURE_DETAILS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.INSERT_DATE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.LAST_FOREIGN_PORT_NAME;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.MODE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.NOTIFY_PARTY;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.ORIGINAL_PRINT_DATE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.PACKS_UNIT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.PCHARGE_UNIT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.PODCODE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.POLCODE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.PVOLUME_UNIT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.RA_CSD;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.ROUTINGS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.SERVICE_LEVEL;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.TOTAL_PACKAGES;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.TOTAL_VOLUME;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.TOTAL_WEIGHT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.USER_INITIALS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.VESSEL_NAME;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.VOYAGE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper.addCommaWithoutDecimal;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper.addTenantDetails;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper.getListOfStrings;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper.getOrgAddressWithPhoneEmail;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.reportingservice.Models.CargoManifestModel;
import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.RoutingsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CargoManifestReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    @Override
    public Map<String, Object> getData(Long id) {
        CargoManifestModel cargoManifestModel = (CargoManifestModel) getDocumentModel(id);
        return populateDictionary(cargoManifestModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();
        cargoManifestModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(cargoManifestModel.shipmentDetails); // check
        cargoManifestModel.tenantDetails = getTenant();
        cargoManifestModel.usersDto = UserContext.getUser();
        cargoManifestModel.awb = getHawb(id);
        cargoManifestModel.shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return cargoManifestModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestModel cargoManifestModel = (CargoManifestModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        addTenantDetails(dictionary, cargoManifestModel.tenantDetails);

        List<String> consigner = new ArrayList<>();
        if(cargoManifestModel.shipmentDetails.getConsigner() != null) {
            consigner = getOrgAddressWithPhoneEmail(cargoManifestModel.shipmentDetails.getConsigner());
            if(cargoManifestModel.shipmentDetails.getConsigner().getOrgData() != null) {
                Map<String, Object> partyOrg = cargoManifestModel.shipmentDetails.getConsigner().getOrgData();
                if(!Boolean.TRUE.equals(cargoManifestModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    consigner.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }

        List<String> consignee = new ArrayList<>();
        if(cargoManifestModel.shipmentDetails.getConsignee() != null) {
            consignee = getOrgAddressWithPhoneEmail(cargoManifestModel.shipmentDetails.getConsignee());
            if(cargoManifestModel.shipmentDetails.getConsignee().getOrgData() != null) {
                Map<String, Object> partyOrg = cargoManifestModel.shipmentDetails.getConsignee().getOrgData();
                if(!Boolean.TRUE.equals(cargoManifestModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    consignee.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }

        List<String> notify = new ArrayList<>();
        if(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null) {
            notify = getOrgAddressWithPhoneEmail(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty());
            if(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData() != null) {
                Map<String, Object> partyOrg = cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData();
                if(!Boolean.TRUE.equals(cargoManifestModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    notify.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }

        List<String> tenantsDataList = getListOfStrings(cargoManifestModel.tenantDetails.tenantName, cargoManifestModel.tenantDetails.address1, cargoManifestModel.tenantDetails.address2,
                cargoManifestModel.tenantDetails.city, cargoManifestModel.tenantDetails.state, cargoManifestModel.tenantDetails.zipPostCode, cargoManifestModel.tenantDetails.country,
                cargoManifestModel.tenantDetails.email, cargoManifestModel.tenantDetails.websiteUrl, cargoManifestModel.tenantDetails.phone);
        if(tenantsDataList != null)
            dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(ReportConstants.CONSIGNOR, consigner);
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, consignee);
        dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
        dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.MAWB_NO, cargoManifestModel.shipmentDetails.getMasterBill());
        dictionary.put(ReportConstants.HAWB_NO, cargoManifestModel.shipmentDetails.getHouseBill());
        dictionary.put(ReportConstants.SHIPMENT_NO, cargoManifestModel.shipmentDetails.getShipmentId());
        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        Set<String> locCodes = new HashSet<>();
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin() != null) {
            locCodes.add(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin());
        }
        dictionary.put(ReportConstants.POL, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()));
        dictionary.put(ReportConstants.POD, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()));
        dictionary.put(ReportConstants.FPOD, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()));
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getEtd() != null) {
            dictionary.put(ReportConstants.ETD_CAPS, ConvertToDPWDateFormat(cargoManifestModel.shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getEta() != null) {
            dictionary.put(ReportConstants.ETA_CAPS, ConvertToDPWDateFormat(cargoManifestModel.shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
        dictionary.put(ReportConstants.FLIGHT_NAME, cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, cargoManifestModel.shipmentDetails.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.PP_CC, cargoManifestModel.shipmentDetails.getPaymentTerms());
        dictionary.put(ReportConstants.BOOKING_NO, cargoManifestModel.shipmentDetails.getBookingNumber());
        if(cargoManifestModel.shipmentDetails.getPackingList() != null && cargoManifestModel.shipmentDetails.getPackingList().size() > 0) {
            var request = cargoManifestModel.shipmentDetails.getPackingList().stream().filter(c -> StringUtility.isNotEmpty(c.getCommodity())).map(PackingModel::getCommodity).toList();
            var v1DataMap = masterDataUtils.fetchInBulkCommodityTypes(request);
            List<Map<String, Object>> packDictionary = new ArrayList<>();
            for (PackingModel pack : cargoManifestModel.shipmentDetails.getPackingList()) {
                var map = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(pack));
                map.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(map.get(ReportConstants.WEIGHT), v1TenantSettingsResponse));
                map.put(ReportConstants.NET_WEIGHT, ConvertToWeightNumberFormat(map.get(ReportConstants.NET_WEIGHT), v1TenantSettingsResponse));
                map.put(ReportConstants.VOLUME_WEIGHT, ConvertToWeightNumberFormat(map.get(ReportConstants.VOLUME_WEIGHT), v1TenantSettingsResponse));
                map.put(ReportConstants.VOLUME, ConvertToVolumeNumberFormat(map.get(ReportConstants.VOLUME), v1TenantSettingsResponse));
                if (v1DataMap.containsKey(pack.getCommodity()))
                    map.put(ReportConstants.COMMODITY_NAME, v1DataMap.get(pack.getCommodity()).getDescription());
                packDictionary.add(map);
            }
            packDictionary.forEach(v -> JsonDateFormat(v));
            dictionary.put(ReportConstants.ITEMS, packDictionary);
        }
        dictionary.put(ReportConstants.CMS_REMARKS, cargoManifestModel.shipmentDetails.getAdditionalTerms());
        dictionary.put(ReportConstants.USER_EMAIL, cargoManifestModel.usersDto.Email);
        dictionary.put(ReportConstants.DATE_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern("dd/MMM/y hh:mm a")));
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(cargoManifestModel.shipmentDetails);
        locCodes.addAll(unlocoRequests);
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = masterDataUtils.getLocationDataFromCache(locCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        for (Map.Entry<String, EntityTransferUnLocations> entry : entityTransferUnLocationsMap.entrySet()) {
            String key = entry.getKey();
            UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
            unlocationsMap.put(key, value);
        }
        UnlocationsResponse origin = unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin());
        if(origin != null)
            dictionary.put(ReportConstants.POR, origin.getNameWoDiacritics());
        try {dictionary.put(ReportConstants.POR_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin()).getName().toUpperCase());} catch (Exception ignored) {}
        try {dictionary.put(ReportConstants.POL_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()).getPortName().toUpperCase());} catch (Exception ignored) {}
        try {dictionary.put(ReportConstants.FPOD_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()).getPortName().toUpperCase());} catch (Exception ignored) {}
        try {dictionary.put(ReportConstants.POD_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()).getName().toUpperCase());} catch (Exception ignored) {}

        try {dictionary.put(ReportConstants.POR_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin()).getCountry().toUpperCase());} catch (Exception ignored) {}
        try {dictionary.put(ReportConstants.POL_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()).getCountry().toUpperCase());} catch (Exception ignored) {}
        try {dictionary.put(ReportConstants.POD_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()).getCountry().toUpperCase());} catch (Exception ignored) {}
        try {dictionary.put(ReportConstants.FPOD_COUNTRY_NAME_IN_CAPS, unlocationsMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()).getCountry().toUpperCase());} catch (Exception ignored) {}

        if(!CommonUtils.IsStringNullOrEmpty(cargoManifestModel.shipmentDetails.getPaymentTerms())) {
            MasterData paymentTerms = getMasterListData(MasterDataType.PAYMENT, cargoManifestModel.shipmentDetails.getPaymentTerms());
            try {dictionary.put(ReportConstants.PAYMENT_TERMS_DESCRIPTION, paymentTerms.getItemDescription());} catch (Exception ignored) {}
            try {dictionary.put(ReportConstants.PAYMENT_TERMS, cargoManifestModel.shipmentDetails.getPaymentTerms());} catch (Exception ignored) {}
        }
        if(!CommonUtils.IsStringNullOrEmpty(cargoManifestModel.shipmentDetails.getPacksUnit())) {
            MasterData packsUnitDesc = getMasterListData(MasterDataType.PACKS_UNIT, cargoManifestModel.shipmentDetails.getPacksUnit());
            String packsUnit = null;
            try {packsUnit = packsUnitDesc.getItemDescription();} catch (Exception ignored) {}
            if(CommonUtils.IsStringNullOrEmpty(packsUnit))
                packsUnit = cargoManifestModel.shipmentDetails.getPacksUnit();
            dictionary.put(ReportConstants.PACKS_UNIT_DESCRIPTION, Constants.MPK.equals(packsUnit) ? Constants.PACKAGES : packsUnit);
        }
        try {
            if(!CommonUtils.IsStringNullOrEmpty(cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine())) {
                Set<String> carrierSet = new HashSet<>();
                carrierSet.add(cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine());
                Map<String, EntityTransferCarrier> entityTransferCarrierMap = masterDataUtils.getCarrierDataFromCache(carrierSet);
                dictionary.put(ReportConstants.FLIGHT_IATA_CODE, entityTransferCarrierMap.get(cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine()).IATACode);
            }
        } catch (Exception ignored) {}
        if(cargoManifestModel.shipmentDetails.getBookingCarriagesList() != null && cargoManifestModel.shipmentDetails.getBookingCarriagesList().size() > 0) {
            Set<String> unlocoStrings = new HashSet<>();
            for (BookingCarriageModel bookingCarriageModel : cargoManifestModel.shipmentDetails.getBookingCarriagesList()) {
                if (bookingCarriageModel.getCarriageType() != null && (bookingCarriageModel.getCarriageType().equals(Constants.PRE_CARRIAGE) || bookingCarriageModel.getCarriageType().equals(Constants.MAIN))) {
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.VESSEL, bookingCarriageModel.getVessel());
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.VOYAGE, bookingCarriageModel.getVoyage());
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.ETD_CAPS, ConvertToDPWDateFormat(bookingCarriageModel.getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
                    dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.ETA_CAPS, ConvertToDPWDateFormat(bookingCarriageModel.getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
                    unlocoStrings.add(bookingCarriageModel.getPortOfLoading());
                    unlocoStrings.add(bookingCarriageModel.getPortOfDischarge());
                    Map<String, EntityTransferUnLocations> entityUnLocationsMap = masterDataUtils.getLocationDataFromCache(unlocoStrings, EntityTransferConstants.LOCATION_SERVICE_GUID);
                    for (Map.Entry<String, EntityTransferUnLocations> entry : entityUnLocationsMap.entrySet()) {
                        String key = entry.getKey();
                        UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
                        unlocationsMap.put(key, value);
                    }
                    UnlocationsResponse pol = unlocationsMap.get(bookingCarriageModel.getPortOfLoading());
                    UnlocationsResponse pod = unlocationsMap.get(bookingCarriageModel.getPortOfDischarge());
                    if (pol != null) {
                        dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_LOAD_COUNTRY, pol.getCountry());
                        dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_LOAD_PORT, pol.getPortName());
                        dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_LOAD_CODE, pol.getLocCode());
                    }
                    if (pod != null) {
                        dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_DISCHARGE_COUNTRY, pod.getCountry());
                        dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_DISCHARGE_PORT, pod.getPortName());
                        dictionary.put(bookingCarriageModel.getCarriageType() + ReportConstants.PLACE_OF_DISCHARGE_CODE, pod.getLocCode());
                    }
                }
            }
        }
        dictionary.put(ReportConstants.CONTAINER_TYPE, cargoManifestModel.shipmentDetails.getShipmentType());
        BigDecimal Total_GrossWeight = BigDecimal.ZERO;
        BigDecimal Total_GrossVolume = BigDecimal.ZERO;
        long Total_ContainerCount = 0;
        long Total_Packs = 0;
        if(cargoManifestModel.shipmentDetails.getContainersList() != null && cargoManifestModel.shipmentDetails.getContainersList().size() > 0) {
            List<ShipmentContainers> shipmentContainersList = new ArrayList<>();
            for (ContainerModel item : cargoManifestModel.shipmentDetails.getContainersList()) {
                ShipmentContainers shipmentContainers = getShipmentContainer(item);
                shipmentContainersList.add(shipmentContainers);
                if(item.getGrossWeight() != null)
                    Total_GrossWeight = Total_GrossWeight.add(item.getGrossWeight());
                if(item.getGrossVolume() != null)
                    Total_GrossVolume = Total_GrossVolume.add(item.getGrossVolume());
                if(item.getContainerCount() != null) {
                    Total_ContainerCount = Total_ContainerCount + item.getContainerCount();
                }
                if(!CommonUtils.IsStringNullOrEmpty(item.getPacks())) {
                    Total_Packs = Total_Packs + Long.parseLong(item.getPacks());
                }
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, shipmentContainersList);
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ShipmentContainers shipmentContainers : shipmentContainersList) {
                valuesContainer.add(jsonHelper.convertValue(shipmentContainers, new TypeReference<>() {}));
            }
            for (Map<String, Object> v : valuesContainer) {
                if(v.containsKey(ReportConstants.GROSS_VOLUME) && v.get(ReportConstants.GROSS_VOLUME) != null)
                    v.put(ReportConstants.GROSS_VOLUME, ConvertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.GROSS_WEIGHT) && v.get(ReportConstants.GROSS_WEIGHT) != null)
                    v.put(ReportConstants.GROSS_WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.SHIPMENT_PACKS) && v.get(ReportConstants.SHIPMENT_PACKS) != null)
                    v.put(ReportConstants.SHIPMENT_PACKS, addCommaWithoutDecimal(new BigDecimal(v.get(ReportConstants.SHIPMENT_PACKS).toString())));
                if (v.containsKey(ReportConstants.TARE_WEIGHT) && v.get(ReportConstants.TARE_WEIGHT) != null)
                    v.put(ReportConstants.TARE_WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.TARE_WEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.VGMWEIGHT) && v.get(ReportConstants.VGMWEIGHT) != null)
                    v.put(ReportConstants.VGMWEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.VGMWEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(CONTAINER_TYPE_CODE))
                    v.put(CONTAINER_TYPE_DESCRIPTION, v.get(CONTAINER_TYPE_CODE));
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
            dictionary.put(COMMON_CONTAINERS, valuesContainer);
            dictionary.put(ReportConstants.TOTAL_CNTR_WEIGHT, ConvertToWeightNumberFormat(Total_GrossWeight, v1TenantSettingsResponse));
            dictionary.put(ReportConstants.TOTAL_CNTR_VOLUME, ConvertToVolumeNumberFormat(Total_GrossVolume, v1TenantSettingsResponse));
            dictionary.put(ReportConstants.TOTAL_CNTR_COUNT, addCommaWithoutDecimal(new BigDecimal(Total_ContainerCount)));
            dictionary.put(ReportConstants.TOTAL_CNTR_PACKS, addCommaWithoutDecimal(new BigDecimal(Total_Packs)));
        }

        if(cargoManifestModel.awb != null) {
            AwbCargoInfo cargoInfoRows = cargoManifestModel.awb.getAwbCargoInfo();
            dictionary.put(ReportConstants.SCI, cargoInfoRows.getSci());
            dictionary.put(RA_CSD, geteCSDInfo(cargoManifestModel.awb));
            dictionary.put(ORIGINAL_PRINT_DATE, getPrintOriginalDate(cargoManifestModel.awb));
            dictionary.put(USER_INITIALS, Optional.ofNullable(cargoInfoRows.getUserInitials()).map(StringUtility::toUpperCase).orElse(StringUtility.getEmptyString()));
        }
        populateRaKcData(dictionary, cargoManifestModel.shipmentDetails);

        if(!listIsNullOrEmpty(cargoManifestModel.shipmentDetails.getPackingList())) {
            getPackingDetails(cargoManifestModel.shipmentDetails, dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = cargoManifestModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = cargoManifestModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
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
        if(cargoManifestModel.shipmentDetails.getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty()));
        }

        if (cargoManifestModel.shipmentDetails.getConsolidationList() != null && !cargoManifestModel.shipmentDetails.getConsolidationList().isEmpty()) {
            ConsolidationModel consol = cargoManifestModel.shipmentDetails.getConsolidationList().get(0);
            dictionary.put(CONSOLIDATION_NUMBER, consol.getConsolidationNumber());
            dictionary.put(AGENT_REFERENCE, consol.getAgentReference());
            if(Constants.IMP.equalsIgnoreCase(cargoManifestModel.shipmentDetails.getDirection())) {
                var ctoAddress = consol.getArrivalDetails() == null ? new ArrayList<>(): ReportHelper.getOrgAddress(consol.getArrivalDetails().getCTOId());
                dictionary.put(CTO_ADDRESS, ctoAddress);
                Set<String> arrivalRequest = new HashSet<>();
                if(consol.getArrivalDetails()!=null){
                    arrivalRequest.add(consol.getArrivalDetails().getLastForeignPort());
                    Map<String, EntityTransferUnLocations> arrivalEntityTransferUnlocMap = masterDataUtils.getLocationDataFromCache(arrivalRequest, EntityTransferConstants.LOCATION_SERVICE_GUID);
                    for (Map.Entry<String, EntityTransferUnLocations> entry : arrivalEntityTransferUnlocMap.entrySet()) {
                        String key = entry.getKey();
                        UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
                        unlocationsMap.put(key, value);
                    }
                    UnlocationsResponse arrival = unlocationsMap.get(consol.getArrivalDetails().getLastForeignPort());
                    if (arrival != null)
                        dictionary.put(LAST_FOREIGN_PORT_NAME, arrival.getLocCode());
                }
            } else {
                var ctoAddress = consol.getDepartureDetails() == null ? new ArrayList<>(): ReportHelper.getOrgAddress(consol.getDepartureDetails().getCTOId());
                dictionary.put(CTO_ADDRESS, ctoAddress);
                Set<String> departureRequest = new HashSet<>();
                if(consol.getDepartureDetails()!=null){
                    departureRequest.add(consol.getDepartureDetails().getLastForeignPort());
                    Map<String, EntityTransferUnLocations> departureEntityTransferUnlocMap = masterDataUtils.getLocationDataFromCache(departureRequest, EntityTransferConstants.LOCATION_SERVICE_GUID);
                    for (Map.Entry<String, EntityTransferUnLocations> entry : departureEntityTransferUnlocMap.entrySet()) {
                        String key = entry.getKey();
                        UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
                        unlocationsMap.put(key, value);
                    }
                    UnlocationsResponse departure = unlocationsMap.get(consol.getDepartureDetails().getLastForeignPort());
                    if (departure != null)
                        dictionary.put(LAST_FOREIGN_PORT_NAME, departure.getLocCode());
                }
            }
        }

        dictionary.put(INSERT_DATE, ConvertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(TOTAL_WEIGHT, ConvertToWeightNumberFormat(cargoManifestModel.shipmentDetails.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.PWEIGHT_UNIT, cargoManifestModel.shipmentDetails.getWeightUnit());
        dictionary.put(TOTAL_VOLUME, ConvertToVolumeNumberFormat(cargoManifestModel.shipmentDetails.getVolume(), v1TenantSettingsResponse));
        dictionary.put(PVOLUME_UNIT, cargoManifestModel.shipmentDetails.getVolumeUnit());
        String chargeableString = ConvertToWeightNumberFormat(cargoManifestModel.shipmentDetails.getChargable(), CHARGEABLE_WEIGHT_DECIMAL_PLACES, v1TenantSettingsResponse);
        dictionary.put(CHARGEABLE, chargeableString);
        dictionary.put(CHARGEABLE_AND_UNIT, String.format(REGEX_S_S, chargeableString, cargoManifestModel.shipmentDetails.getChargeableUnit()));
        dictionary.put(CHARGEABLE_AND_UNIT1, dictionary.get(CHARGEABLE_AND_UNIT));
        dictionary.put(PCHARGE_UNIT, cargoManifestModel.shipmentDetails.getChargeableUnit());
        dictionary.put(TOTAL_PACKAGES, cargoManifestModel.shipmentDetails.getNoOfPacks());
        dictionary.put(PACKS_UNIT, cargoManifestModel.shipmentDetails.getPacksUnit());
        dictionary.put(CARRIER_BOOKING_REF, cargoManifestModel.shipmentDetails.getBookingNumber());
        dictionary.put(SERVICE_LEVEL, cargoManifestModel.shipmentDetails.getServiceType());

        if (cargoManifestModel.shipmentDetails.getRoutingsList() != null && !cargoManifestModel.shipmentDetails.getRoutingsList().isEmpty()) {
            List<RoutingsModel> mainCarriageRouts = cargoManifestModel.shipmentDetails.getRoutingsList().stream()
                    .filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage()))
                    .toList();
            if (!mainCarriageRouts.isEmpty()) {
                List<Map<String, Object>> mainCarriageRoutsList = new ArrayList<>();
                Set<String> vesselGuids = new HashSet<>();
                mainCarriageRouts.stream().filter(e -> !CommonUtils.IsStringNullOrEmpty(e.getVesselName())).forEach(e -> vesselGuids.add(e.getVesselName()));
                Map<String, EntityTransferVessels> vesselsMap = masterDataUtils.fetchInBulkVessels(vesselGuids);
                for (RoutingsModel route : mainCarriageRouts) {
                    Map<String, Object> routeMap = new HashMap<>();
                    routeMap.put(MODE, route.getMode());
                    if(!CommonUtils.IsStringNullOrEmpty(route.getVesselName()) && vesselsMap.containsKey(route.getVesselName()))
                        routeMap.put(VESSEL_NAME, vesselsMap.get(route.getVesselName()).getName());
                    routeMap.put(VOYAGE, route.getVoyage());
                    routeMap.put(CARRIER, route.getCarrier());
                    routeMap.put(POLCODE, route.getPol());
                    routeMap.put(PODCODE, route.getPod());
                    routeMap.put(ETD_FOR_PRINT, ConvertToDPWDateFormat(route.getEtd()));
                    routeMap.put(ETA_FOR_PRINT, ConvertToDPWDateFormat(route.getEta()));
                    mainCarriageRoutsList.add(routeMap);
                }
                dictionary.put(ROUTINGS, mainCarriageRoutsList);
            }
        }

        if (cargoManifestModel.shipmentDetails.getReferenceNumbersList() != null && !cargoManifestModel.shipmentDetails.getReferenceNumbersList().isEmpty()) {
            Optional<ReferenceNumbersModel> refNumberModel = cargoManifestModel.shipmentDetails.getReferenceNumbersList().stream()
                    .filter(i -> ReportConstants.CEN.equals(i.getType()))
                    .reduce((first, second) -> second);
            refNumberModel.ifPresent(referenceNumbersModel -> dictionary.put(CUSTOMS_ENTRY_NUMBER, referenceNumbersModel.getReferenceNumber()));
        }
        return dictionary;
    }

}
