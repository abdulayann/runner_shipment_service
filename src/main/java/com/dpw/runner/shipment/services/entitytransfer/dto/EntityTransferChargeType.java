package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferChargeType implements IEntityTranferBaseEntity {
    public String ChargeCode;
    public String Description;
    public String Services;
    public String ChargeCodeAlt;
    public Double DefaultCost;
    public Double DefaultSell;
    public Double DefaultCurrency;
    public Boolean Taxable;
    public Boolean Hazardous;
}
