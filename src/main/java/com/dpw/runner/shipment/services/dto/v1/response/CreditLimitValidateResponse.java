package com.dpw.runner.shipment.services.dto.v1.response;

import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CreditLimitValidateResponse {
    @JsonProperty("IsValid")
    private Boolean isValid;
    @JsonProperty("TaskRequiredMessage")
    private Boolean taskRequiredMessage;
    @JsonProperty("Message")
    private String message;
    @JsonProperty("Error")
    private V1ErrorResponse.V1Error error;
}
