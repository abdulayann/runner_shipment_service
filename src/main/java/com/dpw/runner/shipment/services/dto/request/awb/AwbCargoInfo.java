package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

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
    @NotBlank(message = "handlingInfo is mandatory")
    private String handlingInfo;
    @Size(max = 5, message = "maximum size is 5 for handling information code")
    private String handlingInfoCode;
    private String otherInfo;
    private String otherInfoCode;
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
    // To be removed in Future Iterations
    private String csdInfo;
    private String csdInfoDate;
    // To be used for Screener's name
    private String userInitials;
    private Integer slac;

    private String countryCode;
    private String raNumber;
    private List<String> screeningStatus;
    private String otherMethod;
    private String exemptionCode;
    private LocalDateTime screeningTime;
    private String securityStatus;
}
