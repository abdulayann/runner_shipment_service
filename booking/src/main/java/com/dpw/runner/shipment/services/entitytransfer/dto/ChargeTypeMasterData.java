package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IMasterDataBaseEntity;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.io.Serializable;
import java.util.List;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ChargeTypeMasterData implements IMasterDataBaseEntity, Serializable {
    public String ChargeCode;
    public String Description;
    public String Services;
    public String ChargeCodeAlt;
    public Double DefaultCost;
    public Double DefaultSell;
    public String DefaultCurrency;
    public Boolean Taxable;
    public Boolean Hazardous;

    @JsonProperty("ChargeTypeIntegrations")
    private List<ChargeTypeIntegrations> ChargeTypeIntegrations;
}
