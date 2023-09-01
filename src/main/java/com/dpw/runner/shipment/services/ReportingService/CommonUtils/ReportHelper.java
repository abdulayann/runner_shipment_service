package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import java.util.ArrayList;
import java.util.List;

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

    public static List<String> getAddressList(String address)
    {
        if(address == null)
            return null;
        return List.of(address.split("\n"));
    }
}
