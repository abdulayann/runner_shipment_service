package com.dpw.runner.shipment.services.dto.v1.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class CreditLimitValidateRequest {
    @JsonProperty("RestrictedItem")
    private String restrictedItem;
    @JsonProperty("ClientId")
    private Integer clientId;
    @JsonProperty("ClientAddressId")
    private Integer clientAddressId;
    @JsonProperty("ShipmentGuid")
    private String shipmentGuid;
    @JsonProperty("TaskCreation")
    private Boolean taskCreation;
}
