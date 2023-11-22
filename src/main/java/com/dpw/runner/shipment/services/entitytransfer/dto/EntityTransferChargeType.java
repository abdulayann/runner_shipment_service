package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferChargeType implements IEntityTranferBaseEntity, Serializable {
    public String ChargeCode;
    public String Description;
    public String Services;
    public String ChargeCodeAlt;
    public Double DefaultCost;
    public Double DefaultSell;
    public String DefaultCurrency;
    public Boolean Taxable;
    public Boolean Hazardous;
}
