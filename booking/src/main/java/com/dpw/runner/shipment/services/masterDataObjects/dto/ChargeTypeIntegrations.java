package com.dpw.runner.shipment.services.masterDataObjects.dto;

import com.dpw.runner.shipment.services.entity.enums.ChargeTypeCode;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;

@Data
public class ChargeTypeIntegrations implements Serializable {
    public String IntegrationCode;
    public ChargeTypeCode IntegrationType;
    @JsonProperty("ChargeDue")
    public Integer ChargeDue;
}
