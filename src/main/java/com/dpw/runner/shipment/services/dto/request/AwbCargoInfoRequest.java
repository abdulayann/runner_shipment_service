package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@ApiModel("Awb Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AwbCargoInfoRequest implements IRunnerRequest {
    private Long entityId;
    private String entityType;
    private String accountingInfo;
    private String handlingInfo;
    private String handlingInfoCode;
    private String otherInfo;
    private String otherInfoCode;
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
    // To be removed in Future Iterations
    private String csdInfo;
    private String csdInfoDate;
    // To be used for Screener's name
    private String userInitials;
    private Integer slac;

    private String countryCode;
    private String raNumber;
    @ExcludeTimeZone
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime raExpiryDate;
    private List<String> screeningStatus;
    private String otherMethod;
    private String exemptionCode;

    private LocalDateTime screeningTime;
    private String securityStatus;
}
