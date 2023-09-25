package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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
        if(getValueFromMap(partyAddress,"CompanyName") != null)
            list.add(getValueFromMap(partyAddress,"CompanyName"));
        if(getValueFromMap(partyAddress,"Address1") != null)
            list.add(getValueFromMap(partyAddress,"Address1"));
        if(getValueFromMap(partyAddress,"Address2") != null)
            list.add(getValueFromMap(partyAddress,"Address2"));
        if(getCityCountry(getValueFromMap(partyAddress,"City"), getValueFromMap(partyAddress,"Country")) != null)
            list.add(getCityCountry(getValueFromMap(partyAddress,"City"), getValueFromMap(partyAddress,"Country")));
        if(getValueFromMap(partyAddress,"Email") != null)
            list.add(getValueFromMap(partyAddress,"Email"));
        if(getValueFromMap(partyAddress,"ContactPhone") != null)
            list.add(getValueFromMap(partyAddress,"ContactPhone"));
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
        if(state_country != null);
            list.add(state_country);
        return list;

    }

    public static List<String> getOrgAddress(PartiesModel party) {
        if(party == null || party.getAddressData() == null)
            return new ArrayList<>();
        Map<String, Object> partyAddress = party.getAddressData();
        List<String> list = new ArrayList<String>();
        if(getValueFromMap(partyAddress,"CompanyName") != null)
            list.add(getValueFromMap(partyAddress,"CompanyName"));
        if(getValueFromMap(partyAddress,"Address1") != null)
            list.add(getValueFromMap(partyAddress,"Address1"));
        if(getValueFromMap(partyAddress,"Address2") != null)
            list.add(getValueFromMap(partyAddress,"Address2"));
        if(getCityCountry(getValueFromMap(partyAddress,"City"), getValueFromMap(partyAddress,"Country")) != null)
            list.add(getCityCountry(getValueFromMap(partyAddress,"City"), getValueFromMap(partyAddress,"Country")));
        if(getValueFromMap(partyAddress,"Email") != null)
            list.add(getValueFromMap(partyAddress,"Email"));
        if(getValueFromMap(partyAddress,"ContactPhone") != null)
            list.add(getValueFromMap(partyAddress,"ContactPhone"));
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
        Object value = dataMap.get(key);
        if(value == null || ! (value instanceof String)) {
            return null;
        }
        return value.toString();
    }

    public static List<String> getListOfStrings(String... strings) {
        List<String> stringList = new ArrayList<>();
        for (String str : strings) {
            stringList.add(str);
        }
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

    public static void JsonDateFormat(Map<String, Object> dictionary) {
        if (dictionary != null) {
            Map<String, Object> dictionaryCopy = new LinkedHashMap<>(dictionary);
            for (Map.Entry<String, Object> entry : dictionaryCopy.entrySet()) {
                Object value = entry.getValue();
                if (value != null && value instanceof LocalDateTime) {
                    LocalDateTime val = (LocalDateTime) value;
                    dictionary.put(entry.getKey(), IReport.ConvertToDPWDateFormat(val));
                }
            }
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

}
