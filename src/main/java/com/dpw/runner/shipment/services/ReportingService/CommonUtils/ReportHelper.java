package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.json.Json;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.util.*;

@Component
public class ReportHelper {

    private static IV1Service v1Service;
    private static JsonHelper jsonHelper;

    @Autowired
    public ReportHelper(IV1Service v1Service, JsonHelper jsonHelper){
        ReportHelper.v1Service = v1Service;
        ReportHelper.jsonHelper = jsonHelper;
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

    public static List<String> getOrgAddressWithPhoneEmail(Parties party) {
        if(party == null || party.getAddressData() == null)
            return null;
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
        if(state_country != null)
            list.add(state_country);
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

    public static UnlocationsResponse getUNLocRow(String UNLocCode) {
        if(UNLocCode == null || UNLocCode.isEmpty())
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
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

    public static String twoDecimalPlacesFormat(String value){
        if(value.isEmpty() || value.isBlank())
            return value;

        DecimalFormat df = new DecimalFormat("#.00");
        return df.format(value);
    }

}
