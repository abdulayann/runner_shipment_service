package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
public class AwbCargoInfoResponse implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String otherInfoCode;
    private String ntrQtyGoods;
    private String handlingInfoCode;
    private String otherInfo;
    private String accountingInfo;
    private String handlingInfo;
    private String referenceNumber;
    private String shippingInformation;
    private String shippingInformationOther;
    private String sci;
    private String currency;
    private String chargeCode;
    private String customOriginCode;
    private String csdInfo;
    private String csdInfoDate;
    private String userInitials;
    private Boolean isUserInitialsManuallyAdded = false;
    private Integer slac;
    private String countryCode;
    private String raNumber;
    @ExcludeTimeZone
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime raExpiryDate;
    private List<String> screeningStatus;
    private String otherMethod;
    private String exemptionCode;
    private BigDecimal carriageValue;
    private BigDecimal customsValue;
    private BigDecimal insuranceAmount;
    private LocalDateTime screeningTime;
    private String securityStatus;
}
