package com.dpw.runner.shipment.services.commons.dto.response.billing;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

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
}
