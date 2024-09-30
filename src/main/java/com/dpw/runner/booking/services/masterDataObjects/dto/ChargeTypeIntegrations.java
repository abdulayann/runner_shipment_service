package com.dpw.runner.booking.services.masterDataObjects.dto;

import com.dpw.runner.booking.services.entity.enums.ChargeTypeCode;
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
