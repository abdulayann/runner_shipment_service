package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Booking Charges Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class BookingChargesRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long bookingId;
    private String seqNo;
    private String chargeType;
    private String measurementBasis;
    private BigDecimal totalUnitCount;
    private String measurementUnit;
    private BigDecimal estimatedCost;
    private BigDecimal localCostAmount;
    private BigDecimal overseasCostAmount;
    private String overseasCostCurrency;
    private String localCostCurrency;
    private BigDecimal currentCostRate;
    private String costRateCurrency;
    private PartiesRequest creditor;
    private BigDecimal costLocalTax;
    private BigDecimal costTaxPercentage;
    private BigDecimal costOverseasTax;
    private BigDecimal costTaxType1;
    private BigDecimal costTaxType2;
    private BigDecimal costTaxType3;
    private BigDecimal costTaxType4;
    private BigDecimal costLineTotal;
    private BigDecimal estimatedRevenue;
    private BigDecimal localSellAmount;
    private BigDecimal overseasSellAmount;
    private String overseasSellCurrency;
    private String localSellCurrency;
    private BigDecimal currentSellRate;
    private String sellRateCurrency;
    private BigDecimal sellExchange;
    private PartiesRequest debtor;
    private BigDecimal localTax;
    private BigDecimal taxPercentage;
    private BigDecimal overseasTax;
    private BigDecimal taxType1;
    private BigDecimal taxType2;
    private BigDecimal taxType3;
    private BigDecimal taxType4;
    private BigDecimal revenueLineTotal;
    private List<UUID> containersUUID;
    private String internalRemarks;
    private String externalRemarks;
}
