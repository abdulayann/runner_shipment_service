package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Payment Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbPaymentInfo {
    private Long entityId;
    private String entityType;
    private BigDecimal weightCharges;
    private BigDecimal valuationCharge;
    private BigDecimal tax;
    private BigDecimal dueAgentCharges;
    private BigDecimal dueCarrierCharges;
    private BigDecimal totalPrepaid;
    private BigDecimal totalCollect;
}
