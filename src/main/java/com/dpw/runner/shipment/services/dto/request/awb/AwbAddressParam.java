package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.*;

@Data
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Generated
public  class AwbAddressParam
{
    private String address1;
    private String address2;
    private String state;
    private String city;
    private String country;
    private String pinCode;
    private String contactNumber;

    public static AwbAddressParam buildRequest(String address1, String address2, String state, String city, String country, String pinCode, String contactNumber) {
        return AwbAddressParam.builder()
                .address1(address1)
                .address2(address2)
                .state(state)
                .city(city)
                .country(country)
                .pinCode(pinCode)
                .contactNumber(contactNumber)
                .build();
    }
}
