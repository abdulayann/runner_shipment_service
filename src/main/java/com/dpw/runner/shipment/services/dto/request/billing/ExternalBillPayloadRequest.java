package com.dpw.runner.shipment.services.dto.request.billing;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.InvoiceBigDecimal2JsonDeserializer;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import jdk.jfr.Description;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

@Data
@Component
public class ExternalBillPayloadRequest implements IRunnerRequest {

    private List<ExternalBillRequest> externalBillRequestList;

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

    @Getter
    @AllArgsConstructor
    public enum TaxType {
        IGST("IGST"), CGST("CGST"), UGST("UGST"), SGST("SGST"), VAT("VAT");
        private final String code;
    }

    @AllArgsConstructor
    @Getter
    public enum ExchangeRateType {
        VENDOR("VENDOR"),
        CUSTOMER("CUSTOMER"),
        REPORT("REPORT"),
        REVENUE_VENDOR("REVENUE_VENDOR");
        private final String value;
    }

    @AllArgsConstructor
    @Getter
    public enum TaxOverrideField {
        PERCENTAGE,
        AMOUNT
    }

    @Data
    @Builder
    public static class ExternalBillRequest implements IRunnerRequest {

        private BillRequest externalBill;
        private List<ExternalBillChargeRequest> externalBillCharges;
        private ExternalBillConfiguration configuration;
    }

    @Data
    @Builder
    public static class BillRequest implements IRunnerRequest {

        private String billId;
        private String jobNumber;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime openDate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
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
        private String clientId;
        @NotEmpty(message = "ClientAddressId can not be empty")
        private String clientAddressId;
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
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
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
        @JsonProperty("QuotedEstimatedMargin")
        private BigDecimal quotedEstimatedMargin;
        @Description("it is same as MarginBillStatus")
        private String approvedProfitPercentageStatus;
        @Description("it is same as ApprovedMargin")
        private BigDecimal approvedProfitPercentage;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime requestedInvoiceDate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime requestedDueDate;
        private String lockedBy;
        private Boolean isLocked;
        private List<DocumentSelectionRequest> documents;
        private String lastLoadJson;
        private List<String> ignoreValidations;
        private Boolean requireEntireResponseObject = Boolean.TRUE;
        private FinanceStatus financeStatus;
        private OperationStatus operationStatus;
        private String financeTransitionReason;
    }

    @Data
    public static class DocumentSelectionRequest implements IRunnerRequest {

        @NotBlank
        private transient List<MultipartFile> files;
        private String docType;
        private String entity;
        private String entityId;
        private String eventCode;
        private Boolean isClientEnabled;
    }

    @Data
    @Builder
    public static class ExternalBillChargeRequest implements IRunnerRequest {

        @NotEmpty(message = "extRefId can not be empty")
        private String extRefId;
        private String guid;
        private boolean postARInvoice;
        private boolean postAPInvoice;
        private APMetaData apMetaData;
        private BillChargesRequest billChargeRequest;
    }

    @Data
    public static class APMetaData implements IRunnerRequest {

        private String referenceInvoiceNumber;
    }

    @Data
    @Builder
    public static class BillChargesRequest implements IRunnerRequest {

        private String id;
        private String guid;
        @NotEmpty(message = "BillId can not be empty")
        private String billId;
        @NotEmpty(message = "ChargeType can not be empty")
        private String chargeTypeId;
        private String chargeTypeGuid;
        private String details;
        private String chargeTypeCode;
        private Integer sequenceNumber;
        private String paymentTypeCode;
        private Boolean isFromConsolidation = false;
        private List<String> containerGuids;
        private String serviceType;
        @Valid
        private BillChargeCostDetailsRequest billChargeCostDetails;
        @Valid
        private BillChargeRevenueDetailsRequest billChargeRevenueDetails;
        private List<String> ignoreValidations;
        private List<String> autoCalculate;
        private String payableLocation;
        private UUID extGuid;
        private String rateSource = "MANUAL";
    }

    @Data
    @Builder
    public static class BillChargeCostDetailsRequest implements IRunnerRequest {

        private String id;
        private String creditorId;
        private String creditorAddressId;
        private String measurementBasis;
        private BigDecimal measurementBasisQuantity;
        private String measurementBasisUnit;//Kg
        private String measurementContainerTypeCode;
        private BigDecimal estimatedAmount;
        private String estimatedAmountCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal unitRate;
        private String unitRateCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal localCostAmount;
        private String localCostCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasCostAmount;
        private String overseasCostCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasTax;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal totalOverseasAmount;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal taxPercentage;
        private String taxCode;
        private String taxMasterCode;
        private Boolean taxMasterOverride;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal taxAmount;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal totalAmount;
        private String totalAmountCurrency;
        private Boolean noTax = false;
        private String jobNumber;// may not be required as we are storing at bill level
        private String invoiceNumber;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime invoiceDate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime dueDate;
        private Boolean isDueDateAutoPopulated;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime documentRecordDate;
        private String comments;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal reportingAmount;
        private String reportExchangeCurrency;
        private String costAccount;
        private String tdsMasterId;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal tdsAmount;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasTdsAmount;
        private BigDecimal tdsPercentage;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private String apDebitAccount;
        private String apCreditAccount;
        private String bankAccount;
        private String chequeNumber;
        private String placeOfSupply;
        private String chequeBook;
        private String payTypes;
        private BigDecimal costCurrencyExchangeUpdate;
        @Valid
        private Set<TaxDetailsRequest> taxDetails;
        @Valid
        private Set<SubTaxDetailsRequest> subTaxDetails;
        @Valid
        private Set<CurrencyExchangeRateDetailsRequest> currencyExchangeRateDetails;
        private Boolean isRcm = false;
        private String stateName;
        private String stateCode;
        private String stateId;
        private String supplierInvoiceReference;
        private String advancePaymentId;
        private BigDecimal advancePaymentBalance;
        private Boolean isTaxPercentageOverride = false;
        private TaxOverrideField taxOverrideField;
        private String extGuid;
        private Boolean isOverrideSystemTax = true;
    }

    @Data
    @Builder
    public static class BillChargeRevenueDetailsRequest implements IRunnerRequest {

        private String debtorId;
        private String debtorAddressId;
        private String measurementBasis;
        private BigDecimal measurementBasisQuantity;
        private String measurementBasisUnit;//Kg
        private String measurementContainerTypeCode;
        private BigDecimal estimatedAmount;
        private String estimatedAmountCurrency;
        private BigDecimal unitRate;
        private String unitRateCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal localSellAmount;
        private String localSellCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal totalAmount;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal taxPercentage;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal taxAmount;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasSellAmount;
        private String overseasSellCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasTax;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal totalOverseasAmount;
        private String taxCode;
        private String taxMasterCode;
        private Boolean taxMasterOverride = false;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal vendorTotalAmount;
        private String totalAmountCurrency;
        private Boolean noTax = false;
        private String taxInvoiceNumber;
        private String originalArInvoiceNo;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime dueDate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime documentRecordDate;
        private String comments;
        private String paymentTermsCode;
        private BigDecimal reportingAmount;
        private String revenueAccount;
        private String placeOfSupply;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime taxDate;
        @Valid
        private Set<TaxDetailsRequest> taxDetails;
        @Valid
        private Set<SubTaxDetailsRequest> subTaxDetails;
        @Valid
        private Set<CurrencyExchangeRateDetailsRequest> currencyExchangeRateDetails;
        private Boolean isRcm = false;
        private String stateName;
        private String stateCode;
        private String stateId;
        private Boolean isTaxPercentageOverride = false;
        private TaxOverrideField taxOverrideField;
        private String extGuid;
        private Boolean isOverrideSystemTax = true;
    }

    @Data
    public static class TaxDetailsRequest implements IRunnerRequest {

        @NotNull(message = "Tax type can not be null")
        private TaxType taxType;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        @NotNull(message = "Tax percentage can not be null")
        private BigDecimal percentage;
        private BigDecimal amount;
        @NotNull(message = "Tax amount can not be null")
        private BigDecimal overseasAmount;
    }

    @Data
    public static class SubTaxDetailsRequest implements IRunnerRequest {

        @NotNull(message = "Sub Tax percentage can not be null")
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal percentage;
        private String taxCode;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal amount;
        @NotNull(message = "Sub Tax amount can not be null")
        private BigDecimal overseasAmount;
        private String action;
        private String description;
    }

    @Data
    public static class CurrencyExchangeRateDetailsRequest implements IRunnerRequest {

        @NotNull(message = "Exchange rate type can not be null")
        private ExchangeRateType type;//VENDOR/CUSTOMER/REPORT/INVOICE
        @NotBlank(message = "Exchange rate type can not be null")
        private String currency;
        @NotNull(message = "currency quantity type can not be null")
        private BigDecimal currencyQuantity;
        @NotBlank(message = "Base currency type can not be null")
        private String baseCurrency;
        @NotNull(message = "Base currency quantity type can not be null")
        private BigDecimal baseCurrencyQuantity;
        private String source;//RUNNER for now
        private Boolean isReciprocalCurrency;
        @NotNull(message = "Exchange rate can not be null")
        private BigDecimal exchangeRate;
    }

    @Data
    @Builder
    public static class ExternalBillConfiguration implements IRunnerRequest {

        private List<String> autoCalculate;
        private List<String> ignoreValidations;
    }

}
