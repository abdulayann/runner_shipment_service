package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ShipmentBillingListResponse {

    private HashMap<UUID, BillingData> data;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class BillingData {
        @JsonProperty("BillStatus")
        private String BillStatus;
        @JsonProperty("JobStatus")
        private String JobStatus;
        @JsonProperty("TotalEstimatedCost")
        private BigDecimal TotalEstimatedCost;
        @JsonProperty("TotalEstimatedRevenue")
        private BigDecimal TotalEstimatedRevenue;
        @JsonProperty("TotalEstimatedProfit")
        private BigDecimal TotalEstimatedProfit;
        @JsonProperty("TotalEstimatedProfitPercent")
        private BigDecimal TotalEstimatedProfitPercent;
        @JsonProperty("TotalCost")
        private BigDecimal TotalCost;
        @JsonProperty("TotalRevenue")
        private BigDecimal TotalRevenue;
        @JsonProperty("TotalProfit")
        private BigDecimal TotalProfit;
        @JsonProperty("TotalProfitPercent")
        private BigDecimal TotalProfitPercent;
        @JsonProperty("TotalPostedCost")
        private BigDecimal TotalPostedCost;
        @JsonProperty("TotalPostedRevenue")
        private BigDecimal TotalPostedRevenue;
        @JsonProperty("TotalPostedProfit")
        private BigDecimal TotalPostedProfit;
        @JsonProperty("TotalPostedProfitPercent")
        private BigDecimal TotalPostedProfitPercent;
        @JsonProperty("WayBillNumber")
        private String WayBillNumber;
        @JsonProperty("Id")
        private Long Id;
    }

}
