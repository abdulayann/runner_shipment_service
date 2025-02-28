package com.dpw.runner.shipment.services.reportingservice.CommonUtils;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.reportingservice.Models.TenantModel;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.google.common.base.Strings;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.*;

@Component
public class ReportHelper {
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

    public static String getFormattedAddress(PartiesModel partiesModel, boolean includeCompanyName) {
        if (partiesModel == null || partiesModel.getAddressData() == null) {
            return null;
        }

        String response = null;
        if (includeCompanyName) {
            response = getNextLineAddress(partiesModel.getAddressData(), ReportConstants.COMPANY_NAME, null);
        }
        response = getNextLineAddress(partiesModel.getAddressData(), ReportConstants.ADDRESS1, response);
        response = getNextLineAddress(partiesModel.getAddressData(), ReportConstants.ADDRESS2, response);

        String temp = getCommaSeparatedAddress(partiesModel.getAddressData(), ReportConstants.CITY, null);
        temp = getCommaSeparatedAddress(partiesModel.getAddressData(), ReportConstants.STATE, temp);
        temp = getCommaSeparatedAddress(partiesModel.getAddressData(), ReportConstants.COUNTRY, temp);
        temp = getCommaSeparatedAddress(partiesModel.getAddressData(), ReportConstants.ZIP_POST_CODE, temp);

        if (!CommonUtils.IsStringNullOrEmpty(temp)) {
            if (response == null) {
                response = temp;
            } else {
                response = response + "\n" + temp;
            }
        }

        return response;
    }


    public static String getNextLineAddress(Map<String, Object> map, String key, String response) {
        String x = getValueFromMap(map, key);
        if(!CommonUtils.IsStringNullOrEmpty(x)){
            if(response == null)
                response = x;
            else
                response = response + "\n" + x;
        }
        return response;
    }

    public static String getCommaSeparatedAddress(Map<String, Object> map, String key, String response) {
        String x = getValueFromMap(map, key);
        if(!CommonUtils.IsStringNullOrEmpty(x)){
            if(response == null)
                response = x;
            else
                response = response + ", " + x;
        }
        return response;
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

    public static List<String> getOrgAddressWithoutPhoneEmail(String name, String address1, String address2, String city, String stateCode,  String pinCode, String countryCode)
    {
        List<String> details = new ArrayList<>();
        details.add(name);
        if(!Strings.isNullOrEmpty(address1)) {
            details.add(address1);
        }
        if(!Strings.isNullOrEmpty(address2)) {
            details.add(address2);
        }

        StringBuilder locationDetails = new StringBuilder();
        if (!Strings.isNullOrEmpty(city)) {
            locationDetails.append(city);
        }
        if (!Strings.isNullOrEmpty(stateCode)) {
            if (!locationDetails.isEmpty()) {
                locationDetails.append(", ");
            }
            locationDetails.append(stateCode);
        }
        if (!Strings.isNullOrEmpty(pinCode)) {
            if (!locationDetails.isEmpty()) {
                locationDetails.append(", ");
            }
            locationDetails.append(pinCode);
        }
        if (!Strings.isNullOrEmpty(countryCode)) {
            if (!locationDetails.isEmpty()) {
                locationDetails.append(", ");
            }
            locationDetails.append(countryCode);
        }

        if (!locationDetails.isEmpty()) {
            details.add(locationDetails.toString());
        }

        return details;
    }

    public static List<String> getOrgAddressWithoutPhoneEmail(PartiesModel party) {
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
        if(getValueFromMap(partyAddress,"Zip_PostCode") != null)
            list.add(getValueFromMap(partyAddress,"Zip_PostCode"));
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

    public static List<String> getOrgAddressDetails(PartiesModel party) {
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
        if(getValueFromMap(partyAddress, CITY) != null) {
            list.add(getValueFromMap(partyAddress, CITY));
        }
        if(getValueFromMap(partyAddress, STATE) != null) {
            list.add(getValueFromMap(partyAddress, STATE));
        }
        if(getValueFromMap(partyAddress,COUNTRY) != null) {
            list.add(getValueFromMap(partyAddress, COUNTRY));
        }
        if(getValueFromMap(partyAddress,ZIP_POST_CODE) != null)
            list.add(getValueFromMap(partyAddress,ZIP_POST_CODE));
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

    public static List<String> getOrgAddressForLesserLines(String address1, String address2, String state, String city, String state_country, String pincode)
    {
        List<String> list = new ArrayList<>();
        if(StringUtility.isNotEmpty(address1))
            list.add(address1);
        if(StringUtility.isNotEmpty(address2))
            list.add(address2);

        StringBuilder sb = new StringBuilder();

        if (StringUtility.isNotEmpty(city))
            sb.append(city).append(" ");
        if (StringUtility.isNotEmpty(state))
            sb.append(state).append(" ");
        if (StringUtility.isNotEmpty(pincode))
            sb.append(pincode).append(" ");
        if (StringUtility.isNotEmpty(state_country))
            sb.append(state_country).append(" ");

        if (StringUtility.isNotEmpty(sb.toString()))
            list.add(sb.toString());

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
        if(getValueFromMap(party.getAddressData(),ZIP_POST_CODE) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.ZIP_POST_CODE));
        if(getValueFromMap(partyAddress,ReportConstants.CONTACT_PHONE) != null)
            list.add(getValueFromMap(partyAddress,ReportConstants.CONTACT_PHONE));
        return list;
    }

    /**
     * @param dictionary : source dictionary
     * @param partiesModel : party model for populating address
     * @param addressReportKey : cargo manifest report key for address
     * Puts party address with email and contact phone as null
     */
    public static void populateCargoManifestPartyAddress(Map<String, Object> dictionary, PartiesModel partiesModel, String addressReportKey) {
        if (partiesModel != null && partiesModel.getAddressData() != null) {
            Map<String, Object> consignerAddress = partiesModel.getAddressData();
            var consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME),
                    getValueFromMap(consignerAddress, ADDRESS1), getValueFromMap(consignerAddress, ADDRESS2),
                    ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                    null, null, getValueFromMap(consignerAddress, ZIP_POST_CODE));

            dictionary.put(addressReportKey, consigner);
        }
    }

    public static List<String> getAddressList(String address)
    {
        if(address == null)
            return Collections.emptyList();
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
        stringList.addAll(Arrays.asList(strings));
        return stringList;
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

    public static String addCommaWithoutDecimal(BigDecimal amount)
    {
        if (amount == null) return null;
        DecimalFormat decimalFormat = new DecimalFormat("#,###");
        return decimalFormat.format(amount);
    }

    public static String addCommasWithPrecision(BigDecimal number, Integer decimalPlaces) {

        if (number != null) {
            if(decimalPlaces == null)
                decimalPlaces = 2;
            try {
                BigDecimal roundedNumber = number.setScale(decimalPlaces, BigDecimal.ROUND_HALF_UP);
                Locale customLocale = Locale.US;
                NumberFormat numberInstance = NumberFormat.getNumberInstance(customLocale);
                numberInstance.setMinimumFractionDigits(decimalPlaces);
                numberInstance.setMaximumFractionDigits(decimalPlaces);

                return numberInstance.format(roundedNumber);
            } catch (Exception e) {
                e.printStackTrace();  // Handle the exception appropriately
            }
        }

        return number != null ? number.toString() : null;
    }

}
