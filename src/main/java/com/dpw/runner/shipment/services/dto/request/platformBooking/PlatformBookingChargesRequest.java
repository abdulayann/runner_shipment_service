package com.dpw.runner.shipment.services.dto.request.platformBooking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Booking Charges Request Model for Platform")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class PlatformBookingChargesRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private String reference_id;
    private Long bookingId;
    @JsonProperty("sequenceNo")
    private String seqNo;
    private String chargeType;
    private String details;
    private String chargeCodeAlt;
    private String hsnMaster;
    private String measurementBasis;
    private String measurementContainerType;
    private Long totalUnitCount;
    private String measurementUnit;
    private String costCurrencyExchangeUpdate;
    private Boolean reciprocalCurrencyCost;
    private BigDecimal estimatedCost;
    private BigDecimal localCostAmount;
    private BigDecimal overseasCostAmount;
    private String overseasCostCurrency;
    private String localCostCurrency;
    private BigDecimal currentCostRate;
    private String costRateCurrency;
    private BigDecimal costExchange;
    private String costAccount;
    private String costComments;
    private PartiesRequest creditor;
    private String costTaxId;
    private LocalDateTime costTaxDate;
    private BigDecimal costLocalTax;
    private BigDecimal costTaxPercentage;
    private BigDecimal costOverseasTax;
    private Boolean costNoGST;
    private BigDecimal costTaxType1;
    private BigDecimal costTaxType2;
    private BigDecimal costTaxType3;
    private BigDecimal costTaxType4;
    private BigDecimal costLineTotal;
    private String sellCurrencyExchangeUpdate;
    private Boolean reciprocalCurrencyRevenue;
    private BigDecimal estimatedRevenue;
    private BigDecimal localSellAmount;
    private BigDecimal overseasSellAmount;
    private String overseasSellCurrency;
    private String localSellCurrency;
    private BigDecimal currentSellRate;
    private String sellRateCurrency;
    private BigDecimal sellExchange;
    private String revenueAccount;
    private String revenueComments;
    private PartiesRequest debtor;
    private String revenueTaxId;
    private LocalDateTime revenueTaxDate;
    private BigDecimal localTax;
    private BigDecimal taxPercentage;
    private BigDecimal overseasTax;
    private Boolean noGST;
    private BigDecimal taxType1;
    private BigDecimal taxType2;
    private BigDecimal taxType3;
    private BigDecimal taxType4;
    private BigDecimal revenueLineTotal;
    private List<UUID> containersUUID;
    private List<BookingContainerRequest> containers;
}
