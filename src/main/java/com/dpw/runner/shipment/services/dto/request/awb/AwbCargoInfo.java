package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;

@Data
@ApiModel("AWB Cargo Info Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AwbCargoInfo {
    private Long entityId;
    private String entityType;
    private String accountingInfo;
    private String handlingInfo;
    private String otherInfo;
    private String ntrQtyGoods;
    private String referenceNumber;
    private String shippingInformation;
    private String shippingInformationOther;
    private String sci;
    private String currency;
    private String chargeCode;
    private BigDecimal carriageValue;
    private BigDecimal customsValue;
    private BigDecimal insuranceAmount;
    private String customOriginCode;
}
