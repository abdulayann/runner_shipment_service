package com.dpw.runner.shipment.services.dto.request.hbl;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Data
@ToString
@Builder
@AllArgsConstructor
@Generated
@JsonInclude(JsonInclude.Include.ALWAYS) // Include all fields, even when empty
public class BLAddressDto {
    private String name;
    private String addressLine1;
    private String addressLine2;
    private String state;
    private String city;
    private String country;
    private String pinCode;

    // Initialize all fields to empty strings to ensure they appear in JSON
    public BLAddressDto() {
        this.name = "";
        this.addressLine1 = "";
        this.addressLine2 = "";
        this.state = "";
        this.city = "";
        this.country = "";
        this.pinCode = "";
    }

}
