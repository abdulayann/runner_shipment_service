package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;


@Data
@Builder
@ApiModel("Booking Charges Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BookingChargesResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long bookingId;
    private String seqNo;
    private String chargeType;
    //    private String details;
//    private String chargeCodeAlt;
//    private String hsnMaster;
    private String measurementBasis;
    //    private String measurementContainerType;
    private BigDecimal totalUnitCount;
    private String measurementUnit;
    //    private String costCurrencyExchangeUpdate;
//    private Boolean reciprocalCurrencyCost;
    private BigDecimal estimatedCost;
    private BigDecimal localCostAmount;
    private BigDecimal overseasCostAmount;
    private String overseasCostCurrency;
    private String localCostCurrency;
    private BigDecimal currentCostRate;
    private String costRateCurrency;
    private BigDecimal costExchange;
    //    private String costAccount;
//    private String costComments;
    private PartiesResponse creditor;
    //    private String costTaxId;
//    private LocalDateTime costTaxDate;
    private BigDecimal costLocalTax;
    private BigDecimal costTaxPercentage;
    private BigDecimal costOverseasTax;
    //    private Boolean costNoGST;
    private BigDecimal costTaxType1;
    private BigDecimal costTaxType2;
    private BigDecimal costTaxType3;
    private BigDecimal costTaxType4;
    private BigDecimal costLineTotal;
    //    private String sellCurrencyExchangeUpdate;
//    private Boolean reciprocalCurrencyRevenue;
    private BigDecimal estimatedRevenue;
    private BigDecimal localSellAmount;
    private BigDecimal overseasSellAmount;
    private String overseasSellCurrency;
    private String localSellCurrency;
    private BigDecimal currentSellRate;
    private String sellRateCurrency;
    private BigDecimal sellExchange;
    //    private String revenueAccount;
    private String revenueComments;
    private PartiesResponse debtor;
    //    private String revenueTaxId;
//    private LocalDateTime revenueTaxDate;
    private BigDecimal localTax;
    private BigDecimal taxPercentage;
    private BigDecimal overseasTax;
    //    private Boolean noGST;
    private BigDecimal taxType1;
    private BigDecimal taxType2;
    private BigDecimal taxType3;
    private BigDecimal taxType4;
    private BigDecimal revenueLineTotal;
    private String externalRemarks;
    private String internalRemarks;
    private Map<String, String> chargeTypeMasterData;
}
