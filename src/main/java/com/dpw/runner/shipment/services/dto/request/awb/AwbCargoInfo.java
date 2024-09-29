package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

@Data
@ApiModel("AWB Cargo Info Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbCargoInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private String accountingInfo;
    @NotNull(message = "handlingInfo is mandatory")
    private String handlingInfo;
    private String otherInfo;
    private String ntrQtyGoods;
    private String referenceNumber;
    private String shippingInformation;
    private String shippingInformationOther;
    private String sci;
    private String currency;
    @DedicatedMasterData
    private String chargeCode;
    private BigDecimal carriageValue;
    private BigDecimal customsValue;
    private BigDecimal insuranceAmount;
    private String customOriginCode;
    private String csdInfo;
    private Integer slac;
}
