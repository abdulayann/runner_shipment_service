package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
@Builder
@ApiModel("AWB Payment Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbPaymentInfo implements Serializable {
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
