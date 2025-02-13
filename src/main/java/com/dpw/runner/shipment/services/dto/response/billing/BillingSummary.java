package com.dpw.runner.shipment.services.dto.response.billing;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class BillingSummary {

    @JsonProperty("totalCount")
    private Integer totalCount;
    @JsonProperty("totalRevenue")
    private Double totalRevenue;
    @JsonProperty("totalCost")
    private Double totalCost;
    @JsonProperty("totalEstimatedCost")
    private BigDecimal totalEstimatedCost;
    @JsonProperty("totalEstimatedRevenue")
    private BigDecimal totalEstimatedRevenue;
    @JsonProperty("totalEstimatedProfit")
    private BigDecimal totalEstimatedProfit;
    @JsonProperty("totalEstimatedProfitPercent")
    private BigDecimal totalEstimatedProfitPercent;
    @JsonProperty("totalProfit")
    private BigDecimal totalProfit;
    @JsonProperty("totalProfitPercent")
    private BigDecimal totalProfitPercent;
    @JsonProperty("totalPostedCost")
    private BigDecimal totalPostedCost;
    @JsonProperty("totalPostedRevenue")
    private BigDecimal totalPostedRevenue;
    @JsonProperty("totalPostedProfit")
    private BigDecimal totalPostedProfit;
    @JsonProperty("totalPostedProfitPercent")
    private BigDecimal totalPostedProfitPercent;
    @JsonProperty("accruedRevenue")
    private Double accruedRevenue;
    @JsonProperty("accruedCost")
    private Double accruedCost;
    @JsonProperty("invoicedRevenue")
    private Double invoicedRevenue;
    @JsonProperty("invoicedCost")
    private Double invoicedCost;
    @JsonProperty("disbursementAccruedRevenue")
    private Double disbursementAccruedRevenue;
    @JsonProperty("disbursementAccruedCost")
    private Double disbursementAccruedCost;
    @JsonProperty("disbursementInvoicedRevenue")
    private Double disbursementInvoicedRevenue;
    @JsonProperty("disbursementInvoicedCost")
    private Double disbursementInvoicedCost;
    @JsonProperty("disbursementRevenue")
    private Double disbursementRevenue;
    @JsonProperty("disbursementCost")
    private Double disbursementCost;
    @JsonProperty("cumulativeGP")
    private Double cumulativeGP;
    @JsonProperty("cumulativeGPPercentage")
    private Double cumulativeGPPercentage;
    @JsonProperty("moduleGuid")
    private String moduleGuid;
    @JsonProperty("branchId")
    private String branchId;
}


