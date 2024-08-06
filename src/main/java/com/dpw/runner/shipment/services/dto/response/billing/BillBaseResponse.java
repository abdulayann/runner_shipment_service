package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BillBaseResponse implements IRunnerResponse {

    private String billId;
    private Boolean isReadOnlyBill;
    private UUID guId;
    private String moduleId;
    private String moduleIdV1;
    private String moduleRef;
    private String moduleTypeCode;
    private String jobNumber;
    private String localReferenceNo;
    private Boolean isActive;
    private LocalDateTime openDate;
    private LocalDateTime closeDate;
    private LocalDateTime createdDate;
    private LocalDateTime createdAt;
    private String jobStatus;
    private String remarks;
    private Boolean isExempted;
    private Boolean isHiplHrBilling;
    private Boolean isLocked;
    private String lockedBy;
    private BigDecimal profitPercentage;
    private BigDecimal totalCost;
    private BigDecimal totalRevenue;
    private BigDecimal profit;
    private BigDecimal totalEstimatedCost;
    private BigDecimal totalEstimatedRevenue;
    private BigDecimal estimatedProfit;
    private BigDecimal estimatedProfitPercentage;
    private String clientId;
    private String clientName;
    private String clientAddressId;
    private BillAddressResponse clientAddress;
    private String overseasAgentId;
    private String overseasAgentName;
    private Integer overseasAgentAddressId;
    private BillAddressResponse overseasAgentAddress;
    private List<BillChargesLiteResponse> billChargesResponse;
    private CurrencySummaryResponse currencySummaryResponse;
    private UnPostedBillChargesSummaryResponse unPostedSummaryResponse;
    private BillMappingBaseResponse billModuleResponse;
    private String lastLoadJson;
    private String shipmentNo;
    private String shipmentId;
    private List<String> TILinkedBills;
    private Integer maxSequenceNumber;
    private Boolean isReopenCreated;
    private String rejectionRemarks;
    private FinanceStatus financeStatus;
    private OperationStatus operationStatus;
    private Boolean isAutoSellRequired;

    @Getter
    @AllArgsConstructor
    public enum FinanceStatus {
        BLANK("BLANK"),
        OPEN("OPEN"),
        ACCRUED("ACCRUED"),
        DEFERRED("DEFERRED"),
        CLOSED("CLOSED"),
        WIP("WIP"),
        OPS_APPROVED("OPS_APPROVED"),
        MANAGER_APPROVED("MANAGER_APPROVED"),
        F_CLOSED("F_CLOSED");
        private final String value;

        public static Optional<FinanceStatus> getEnumTypeByValue(String value) {
            return Arrays.stream(FinanceStatus.values())
                    .filter(e -> e.getValue().equals(value)).findFirst();
        }
    }

    @Getter
    @AllArgsConstructor
    public enum OperationStatus {
        BLANK("BLANK"),
        WIP("WIP"),
        OPS_APPROVED("OPS_APPROVED"),
        MANAGER_APPROVED("MANAGER_APPROVED"),
        PS_VALIDATED("PS_VALIDATED");

        private final String value;
    }

    @Data
    public static class BillAddressResponse implements IRunnerResponse {

        private String companyName;
        private String state;
        private String country;
        private String shortCode;
        private String city;
        private String addressLine1;
        private String addressLine2;
    }

    @Data
    public static class CurrencySummaryResponse implements IRunnerResponse {

        Map<String, List<CurrencySummaryBaseResponse>> currencySummary;
    }

    @Data
    public static class CurrencySummaryBaseResponse implements IRunnerResponse {

        private String currency;
        private BigDecimal amount;
        private BigDecimal tax;
        private BigDecimal totalAmount;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UnPostedBillChargesSummaryResponse implements IRunnerResponse {

        private BigDecimal unPostedtotalCost;
        private BigDecimal unPostedtotalRevenue;
        private BigDecimal unPostedProfit;
        private BigDecimal unPostedProfitPercentage;
    }

    @Data
    public static class BillChargesLiteResponse implements IRunnerResponse {

        private String id;
        private String chargeTypeDescription;
        private String arInvoiceNumber;
        private String moduleTypeCode;
        private BillChargeRevenueDetailsLiteResponse revenueDetailsResponse;
        private BillChargeCostDetailsLiteResponse costDetailsResponse;
    }

    @Data
    public static class BillChargeCostDetailsLiteResponse implements IRunnerResponse {

        private String id;
        private boolean isPosted;
        private String invoiceNumber;
        private BigDecimal estimatedCost;
        private LocalDateTime invoiceDate;
        private LocalDateTime dueDate;
        private String creditorName;
        private String currency;
        private BigDecimal exchangeRate;
        //Cost Amount in tenant currency
        private BigDecimal localCostAmount;
        //Cost Amount in creditor/vendor/supplier currency
        private BigDecimal overseasCostAmount;
        private BigDecimal overseasTaxAmount;
        private Boolean isOverrideSystemTax;
    }

    @Data
    public static class BillChargeRevenueDetailsLiteResponse implements IRunnerResponse {

        private String id;
        private boolean isPosted;
        private String invoiceNumber;
        private BigDecimal estimatedRevenue;
        private LocalDateTime dueDate;
        private String debtorName;
        //debtor/Customer/BillToParty invoice currency
        private String currency;
        private BigDecimal exchangeRate;
        //Revenue Amount in tenant currency
        private BigDecimal localSellAmount;
        //Revenue Amount in debtor/Customer/BillToParty currency
        private BigDecimal overseasSellAmount;
        private BigDecimal overseasTaxAmount;
        private Boolean isOverrideSystemTax;
    }


}
