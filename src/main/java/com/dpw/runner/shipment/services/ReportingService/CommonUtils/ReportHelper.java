package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IHblRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Component
public class ReportHelper {

    private static IV1Service v1Service;
    private static JsonHelper jsonHelper;
    private static IHblRepository hblRepository;

    @Autowired
    public ReportHelper(IV1Service v1Service, JsonHelper jsonHelper, IHblRepository hblRepository){
        ReportHelper.v1Service = v1Service;
        ReportHelper.jsonHelper = jsonHelper;
        ReportHelper.hblRepository = hblRepository;
    }
    public static String getCityCountry(String city, String country)
    {
        if (city == null)
        {
            if (country == null)
                return null;
            else
                return country;
        }
        else
        {
            if (country == null)
                return city;
            else
                return city + " " + country;
        }
    }

    public static List<String> getOrgAddressWithPhoneEmail(String name, String address1, String address2, String city_country, String email, String phone, String pincode)
    {
        List<String> list = new ArrayList<String>();
        if(name != null)
            list.add(name);
        if(address1 != null)
            list.add(address1);
        if(address2 != null)
            list.add(address2);
        if(city_country != null)
            list.add(city_country);
        if(email != null)
            list.add(email);
        if(phone != null)
            list.add(phone);
        if(pincode != null)
            list.add(pincode);
        return list;
    }

    public static List<String> getOrgAddressWithPhoneEmail(PartiesModel party) {
        if(party == null || party.getAddressData() == null)
            return new ArrayList<>();
        Map<String, Object> partyAddress = party.getAddressData();
        List<String> list = new ArrayList<String>();
        if(getValueFromMap(partyAddress,ReportConstants.COMPANY_NAME) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.COMPANY_NAME));
        if(getValueFromMap(partyAddress,ReportConstants.ADDRESS1) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.ADDRESS1));
        if(getValueFromMap(partyAddress,ReportConstants.ADDRESS2) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.ADDRESS2));
        if(getCityCountry(getValueFromMap(partyAddress,ReportConstants.CITY), getValueFromMap(partyAddress,ReportConstants.COUNTRY)) != null)
            list.add(getCityCountry(getValueFromMap(partyAddress,ReportConstants.CITY), getValueFromMap(partyAddress,ReportConstants.COUNTRY)));
        if(getValueFromMap(partyAddress,ReportConstants.EMAIL) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.EMAIL));
        if(getValueFromMap(partyAddress,ReportConstants.CONTACT_PHONE) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.CONTACT_PHONE));
        if(getValueFromMap(partyAddress,"Zip_PostCode") != null)
            list.add(getValueFromMap(partyAddress,"Zip_PostCode"));
        return list;
    }

    public static List<String> getOrgAddress(String name, String address1, String address2, String city_country, String city_zipcode, String state_country)
    {
        List<String> list = new ArrayList<>();
        if(name != null)
            list.add(name);
        if(address1 != null)
            list.add(address1);
        if(address2 != null)
            list.add(address2);
        if(city_country != null)
            list.add(city_country);
        if(city_zipcode != null)
            list.add(city_zipcode);
        if(state_country != null)
            list.add(state_country);
        return list;

    }

    public static List<String> getOrgAddress(PartiesModel party) {
        if(party == null || party.getAddressData() == null)
            return new ArrayList<>();
        Map<String, Object> partyAddress = party.getAddressData();
        List<String> list = new ArrayList<String>();
        if(getValueFromMap(partyAddress,ReportConstants.COMPANY_NAME) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.COMPANY_NAME));
        if(getValueFromMap(partyAddress,ReportConstants.ADDRESS1) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.ADDRESS1));
        if(getValueFromMap(partyAddress,ReportConstants.ADDRESS2) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.ADDRESS2));
        if(getCityCountry(getValueFromMap(partyAddress,ReportConstants.CITY), getValueFromMap(partyAddress,ReportConstants.COUNTRY)) != null)
            list.add(getCityCountry(getValueFromMap(partyAddress,ReportConstants.CITY), getValueFromMap(partyAddress,ReportConstants.COUNTRY)));
        if(getValueFromMap(partyAddress,ReportConstants.EMAIL) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.EMAIL));
        if(getValueFromMap(partyAddress,ReportConstants.CONTACT_PHONE) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.CONTACT_PHONE));
        return list;
    }

    public static List<String> getAddressList(String address)
    {
        if(address == null)
            return null;
        return List.of(address.split("\n"));
    }

    public static void addTenantDetails(Map<String, Object> dictionary, TenantModel tenantModel) {
        dictionary.put(ReportConstants.TENANT_NAME, tenantModel.tenantName);
        dictionary.put(ReportConstants.TENANT_ADDRESS_1, tenantModel.address1);
        dictionary.put(ReportConstants.TENANT_ADDRESS_2, tenantModel.address2);
        dictionary.put(ReportConstants.TENANT_EMAIL, tenantModel.email);
        dictionary.put(ReportConstants.TENANT_CITY, tenantModel.city);
        dictionary.put(ReportConstants.TENANT_STATE, tenantModel.state);
        dictionary.put(ReportConstants.TENANT_COUNTRY, tenantModel.country);
        dictionary.put(ReportConstants.TENANT_COUNTRY_PHONE, tenantModel.phone);
        dictionary.put(ReportConstants.TENANT_MOBILE, tenantModel.mobile);
        dictionary.put(ReportConstants.TENANT_ZIP_POST_CODE, tenantModel.zipPostCode);
        dictionary.put(ReportConstants.TENANT_URL, tenantModel.websiteUrl);
    }

    public static String getValueFromMap(Map<String, Object> dataMap, String key) {
        if (dataMap == null)
            return null;
        Object value = dataMap.get(key);
        if (value == null || !(value instanceof String)) {
            return null;
        }
        return value.toString();
    }

    public static List<String> getCompleteNameAndAddress(String name, List<String> list) {
        List<String> res = getListOfStrings(name);
        res.addAll(list);
        return res;
    }

    public static List<String> getListOfStrings(String... strings) {
        List<String> stringList = new ArrayList<>();
        for (String str : strings) {
            stringList.add(str);
        }
        return stringList;
    }

    public static UnlocationsResponse getUNLocRow(String UNLocCode) {
        if(UNLocCode == null || UNLocCode.isEmpty())
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList("LocationsReferenceGUID"),
                "=",
                UNLocCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            return unlocationsResponse.get(0);
        return null;
    }

    public static String combineStringsWithComma(String str1, String str2)
    {
        if (str1 == null)
        {
            if (str2 == null) return null;
            else return str2;
        }
        else
        {
            if (str2 == null) return str1;
            else return str1 + ", " + str2;
        }
    }

    public static String getPortDetails(String UNLocCode) {
        UnlocationsResponse unlocationsResponse = getUNLocRow(UNLocCode);
        if(unlocationsResponse != null) {
            return combineStringsWithComma(unlocationsResponse.getName(), unlocationsResponse.getCountry());
        }
        return "";
    }

    public static String twoDecimalPlacesFormat(String value){
        if(value.isEmpty() || value.isBlank())
            return value;

        DecimalFormat df = new DecimalFormat("#.00");
        return df.format(Double.valueOf(value));
    }

    public static String GenerateFormattedDate(LocalDateTime localDateTime, String pattern){
        if(localDateTime == null || pattern == null)
            return null;
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(pattern);
        return dateTimeFormatter.format(localDateTime);
    }

    public static void AddBlDetails(Map<String, Object> dictionary, Long shipmentId) {
        List<Hbl> hbl = hblRepository.findByShipmentId(shipmentId);
        if(hbl != null && hbl.size() > 0){
            dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION,
                    hbl.get(0).getHblData().getCargoTermsDescription());
            dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION,
                    hbl.get(0).getHblData().getBlRemarksDescription());
        }
    }


    public static String numberToWords(Integer numb) {
        if (numb == null)
            return "";
        int number = numb;
        if (number == 0)
            return "zero";
        if (number < 0)
            return "minus " + numberToWords(Math.abs(number));
        String words = "";
        if ((number / 1000000) > 0) {
            words += numberToWords(number / 1000000) + " million ";
            number %= 1000000;
        }
        if ((number / 1000) > 0) {
            words += numberToWords(number / 1000) + " thousand ";
            number %= 1000;
        }
        if ((number / 100) > 0) {
            words += numberToWords(number / 100) + " hundred ";
            number %= 100;
        }
        if (number > 0) {
            if (!words.isEmpty())
                words += "and ";
            String[] unitsMap = {
                    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                    "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
            };
            String[] tensMap = {
                    "zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"
            };
            if (number < 20)
                words += unitsMap[number];
            else {
                words += tensMap[number / 10];
                if ((number % 10) > 0)
                    words += "-" + unitsMap[number % 10];
            }
        }
        return words;
    }

    public static String concatGroupedContainerCount(Map<String, Long> containerCountGrouped) {
        String containerCount = "";
        if (Objects.isNull(containerCountGrouped))
            return "0";
        for (Map.Entry<String, Long> entry : containerCountGrouped.entrySet()) {
            if (!containerCount.isEmpty()) {
                containerCount += " & ";
            }
            containerCount += entry.getValue() + " X " + entry.getKey();
        }
        return containerCount.isEmpty() ? "0" : containerCount;
    }

    public static String concatGroupedFieldValues(Map<String, Double> fieldValuesGrouped, int decimalPlaces) {
        String fieldValue = "";
        if (Objects.isNull(fieldValuesGrouped))
            return "0";
        for (Map.Entry<String, Double> entry : fieldValuesGrouped.entrySet()) {
            double value = Math.round(entry.getValue() * Math.pow(10, decimalPlaces)) / Math.pow(10, decimalPlaces);
            if (!fieldValue.isEmpty()) {
                fieldValue += " & ";
            }
            fieldValue += value + " X " + entry.getKey();
        }
        return fieldValue.isEmpty() ? "0" : fieldValue;
    }

    public static String concatGroupedFields(Map<String, Double> fieldMap, int decimalPlaces) {
        String fieldValue = "";
        if (Objects.isNull(fieldMap))
            return "0";
        for (Map.Entry<String, Double> entry : fieldMap.entrySet()) {
            double value = Math.round(entry.getValue() * Math.pow(10, decimalPlaces)) / Math.pow(10, decimalPlaces);
            if (!fieldValue.isEmpty()) {
                fieldValue += " & ";
            }
            fieldValue += value + " " + entry.getKey();
        }
        return fieldValue.isEmpty() ? "0" : fieldValue;
    }

    public static Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (Objects.isNull(locCodes))
            return locationMap;
        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    List.of("LocationsReferenceGUID"),
                    "In",
                    List.of(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {
                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocationsReferenceGUID(), unlocation);
                }
            }
        }
        return locationMap;
    }

    public static String addCommaWithoutDecimal(BigDecimal amount)
    {
        if (amount == null) return null;
        DecimalFormat decimalFormat = new DecimalFormat("#,###");
        return decimalFormat.format(amount);
    }

    public static EntityTransferDGSubstance fetchDgSubstanceRow(Integer dgSubstanceId) {
        var dgSubstanceRow = new EntityTransferDGSubstance();
        if(dgSubstanceId == null)
            return dgSubstanceRow;
        List<Object> criteria = Arrays.asList(List.of("Id"), "=", dgSubstanceId);
        CommonV1ListRequest listRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchDangerousGoodData(listRequest);

        if(v1DataResponse.entities != null) {
            dgSubstanceRow = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferDGSubstance.class).get(0);
        }

        return dgSubstanceRow;
    }

    public static String ConvertToVolumeNumberFormat(BigDecimal volume) {
        if(volume == null)
            return "";
        int numberDecimalDigits = ShipmentSettingsDetailsContext.getCurrentTenantSettings().getVolumeDecimalPlace();
        return GetDPWWeightVolumeFormat(volume, numberDecimalDigits);
    }

    public static String ConvertToWeightNumberFormat(BigDecimal weight) {
        if(weight == null)
            return "";
        int numberDecimalDigits = ShipmentSettingsDetailsContext.getCurrentTenantSettings().getWeightDecimalPlace();
        return GetDPWWeightVolumeFormat(weight, numberDecimalDigits);
    }

    private static String GetDPWWeightVolumeFormat(BigDecimal weight, int numberDecimalDigits)
    {
        var tenantSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
//            if (tenantSettings.getWVDigitGrouping() && tenantSettings.getWVGroupingNumber())
//            {
//                char customThousandsSeparator = ',';
//                char customDecimalSeparator = '.';
//
//                // if (tenantSettings.WVGroupingNumber == DPWBooking.Modules.Default.GroupingNumber.ApostropheAndDot)
//                // {
//                //     customThousandsSeparator = '\'';
//                // }
//                // else
//                if (tenantSettings.WVGroupingNumber == DPWBooking.Modules.Default.GroupingNumber.DotAndComma)
//                {
//                    customThousandsSeparator = '.';
//                    customDecimalSeparator = ',';
//                }
//
//                var dynamicGroupSizes = tenantSettings.WVDigitGrouping.Value == DPWBooking.Modules.Default.DigitGrouping.THREE ? new[] { 3, 3 } : new[] { 3, 2 };
//
//                var customCulture = new CultureInfo("en-US")
//                {
//                    NumberFormat =
//                    {
//                        NumberDecimalSeparator = customDecimalSeparator.ToString(),
//                                NumberGroupSeparator = customThousandsSeparator.ToString(),
//                                NumberDecimalDigits = numberDecimalDigits,
//                                NumberGroupSizes = dynamicGroupSizes
//                    }
//                };
//
//                return weight?.ToString("N", customCulture) ?? null;
//            }
//            else
//            {
                return IReport.addCommas(weight);
//            }
    }

}
