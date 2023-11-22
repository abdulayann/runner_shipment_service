package com.dpw.runner.shipment.services.dto.request;


import jdk.jfr.Description;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
@Getter
@Setter
public class BillRequest implements Serializable {
    private String billId;
    private String jobNumber;
    private LocalDateTime openDate;
    private LocalDateTime closeDate;
    private String jobStatus;
    @NotEmpty(message = "LocalRef can not be empty")
    private String localReferenceNo;
    private String remarks;
    private Boolean isHiplHrBilling;
    private String moduleType;
    private String moduleRef;
    private String moduleGuid;
    @NotEmpty(message = "ClientId can not be empty")
    private long clientId;
    @NotEmpty(message = "ClientAddressId can not be empty")
    private long clientAddressId;
    private String overseasAgentId;
    private String overseasAgentAddressId;
    private Set<String> referencedQuoteIds;
    private BigDecimal profit;
    private BigDecimal profitPercentage;
    private BigDecimal totalCost;
    private BigDecimal totalRevenue;
    private BigDecimal totalEstimatedCost;
    private BigDecimal totalEstimatedRevenue;
    private BigDecimal estimatedProfit;
    private BigDecimal estimatedProfitPercentage;
    private Boolean actualInvoiceDateEnabled;
    private Boolean manualInvoiceDateEnabled;
    private LocalDateTime invoiceDate;
    private BigDecimal totalUnPostedCost;
    private BigDecimal totalUnPostedRevenue;
    private BigDecimal totalUnPostedProfit;
    private BigDecimal totalUnPostedProfitPercentage;
    private BigDecimal totalQuotedCost;
    private BigDecimal totalQuotedRevenue;
    private BigDecimal quotedProfit;
    private BigDecimal quotedMargin;
    private BigDecimal quotedTotalEstimatedCost;
    private BigDecimal quotedTotalEstimatedRevenue;
    private BigDecimal quotedEstimatedProfit;
    private BigDecimal QuotedEstimatedMargin;
    @Description("it is same as MarginBillStatus")
    private String approvedProfitPercentageStatus;
    @Description("it is same as ApprovedMargin")
    private BigDecimal approvedProfitPercentage;
    private LocalDateTime requestedInvoiceDate;
    private LocalDateTime requestedDueDate;
    private String lockedBy;
    private Boolean isLocked;
    private List<DocumentSelectionRequest> documents;
}
