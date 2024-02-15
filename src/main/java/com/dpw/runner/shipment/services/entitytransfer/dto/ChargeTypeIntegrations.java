package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entity.enums.ChargeTypeCode;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class ChargeTypeIntegrations {
    public String IntegrationCode;
    public ChargeTypeCode IntegrationType;
    @JsonProperty("ChargeDue")
    public Integer ChargeDue;
}
