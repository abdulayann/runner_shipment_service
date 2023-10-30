package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class UpdateOrgCreditLimitBookingResponse {
    @JsonProperty("Success")
    private Boolean success;
    @JsonProperty("CustomerIdentifier")
    private String customerIdentifier;
    @JsonProperty("Error")
    private String error;
}
