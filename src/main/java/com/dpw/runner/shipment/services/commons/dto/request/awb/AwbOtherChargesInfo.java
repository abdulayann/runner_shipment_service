package com.dpw.runner.shipment.services.commons.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Other Charges Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbOtherChargesInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private String chargeTypeId;
    private BigDecimal rate;
    private Integer chargeBasis;
    private BigDecimal amount;
    private String modeOfPayment;
    private Integer chargeDue;
    private String iataDescription;
    private String chargeTypeDescription;
    private BigDecimal awbChargeCodeDefaultVat;
    private Long v2ChargeId;
    private UUID guid;
    private UUID chargeTypeGuid;
}
