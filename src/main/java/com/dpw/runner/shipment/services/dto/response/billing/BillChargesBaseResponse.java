package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class BillChargesBaseResponse implements IRunnerResponse {

    private String id;
    private String guId;
    private String chargeTypeId;
    private String chargeTypeDescription;
    private String details;
    private Boolean isActive;
    private Integer sequenceNumber;
    private String paymentTypeCode;
    private String paymentTypeDescription;
    private Boolean isArPosted;
    private Boolean isApPosted;
    private Integer contractId;
    private Integer tariffId;
    private String quoteNumber;
    private String originBillChargeId;
    private Boolean isFromConsolidation;
    private BillChargeCostDetailsResponse billChargeCostDetails;
    private BillChargeRevenueDetailsResponse billChargeRevenueDetails;
    private ChargeTypeBaseResponse chargeTypeDetails;
    private String moduleTypeCode;
    private String moduleGuid;
    private String serviceType;
    private String moduleRef;
    private String wayBill;
    private String houseBill;
    private String masterBill;
    private String orderNumber;
    private String transportMode;
    private String shipmentType;
    private String arInvoiceNumber;
    private String arOriginalInvoiceNumber;
    private String apInvoiceNumber;
    private String taxInvoiceNumber;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime moduleInsertDate;
    private String shipper;
    private String consignee;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime etd;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime eta;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime createdAt;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime updatedAt;
    private String billId;
    private String billGuid;
    private String payableLocation;

    @JsonIgnore
    private String moduleRefGuid; //module id is the guid for module(TI/SHIP/CONS)
    private Boolean isFmcPosted;
    private BillMappingBaseResponse moduleData;
    private UUID extGuid;
    private String rateSource;

    @Getter
    @AllArgsConstructor
    public enum TaxType {
        IGST, CGST, UGST, SGST, VAT
    }

    @Getter
    @AllArgsConstructor
    public enum ExchangeRateType {
        VENDOR("VENDOR"),
        CUSTOMER("CUSTOMER"),
        REPORT("REPORT"),
        REVENUE_VENDOR("REVENUE_VENDOR");
        private final String value;
    }

    @Getter
    @Setter
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class BillChargeRevenueDetailsResponse implements IRunnerResponse {

        private String debtorId;
        private String debtorAddressId;
        private String debtorName;
        private String debtorAddress;
        private String debtorAddressCompanyName;
        private String measurementBasis;//Weight
        private BigDecimal measurementBasisQuantity;
        private String measurementBasisUnit;//Kg
        private String measurementContainerTypeCode;
        private BigDecimal unitRate;
        private String unitRateCurrency;
        private BigDecimal estimatedAmount;
        private String estimatedAmountCurrency;
        private BigDecimal localSellAmount;
        private String localSellCurrency;
        private BigDecimal overseasSellAmount;
        private BigDecimal totalOverseasAmount;
        private BigDecimal overseasTax;
        private String overseasSellCurrency;
        private BigDecimal vendorTotalAmount;
        private String vendorTotalCurrency;
        private BigDecimal totalAmount;
        private String totalAmountCurrency;
        private BigDecimal taxPercentage;
        private String placeOfSupply;
        private BigDecimal taxAmount;
        private String taxAmountCurrency;
        private String taxCode;
        private String taxMasterCode;
        private String taxInvoiceNumber;
        private String originalArInvoiceNo;
        private boolean noTax;
        private boolean taxMasterOverride;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime dueDate;
        private String comments;
        private String paymentTermsCode;
        private String paymentTermsDescription;
        private BigDecimal reportingAmount;
        private String revenueAccount;
        private Boolean isRcm;
        private String stateName;
        private String stateCode;
        private String stateId;

        @JsonIgnore
        private String measurementContainerTypeId;
        private Boolean isTaxPercentageOverride;
        private Boolean isOverrideSystemTax;
        private String billChargesStatus;

        private List<CurrencyExchangeRateDetailsResponse> currencyExchangeRateDetails;
        private List<TaxDetailsResponse> taxDetails;
    }

    @Getter
    @Setter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class TaxDetailsResponse implements Serializable {

        private TaxType taxType;
        private BigDecimal percentage;
        private BigDecimal amount;
        private BigDecimal overseasAmount;
    }

    @Getter
    @Setter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CurrencyExchangeRateDetailsResponse implements Serializable {

        private ExchangeRateType type;//VENDOR/CUSTOMER/REPORT/REVENUE_VENDOR
        private String currency;
        private BigDecimal currencyQuantity;
        private String baseCurrency;
        private BigDecimal baseCurrencyQuantity;
        private String source;
        private BigDecimal exchangeRate;
        private Boolean isReciprocalCurrency;
    }

    @Getter
    @Setter
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class BillChargeCostDetailsResponse implements IRunnerResponse {

        private String id;
        private String creditorId;
        private String creditorName;
        private String creditorAddressId;
        private String creditorAddress;
        private String creditorAddressCompanyName;
        private String measurementBasis;//Weight
        private BigDecimal measurementBasisQuantity;
        private String measurementBasisUnit;//Kg
        private String measurementContainerTypeCode;
        private BigDecimal estimatedAmount;
        private String estimatedAmountCurrency;
        private BigDecimal unitRate;
        private String unitRateCurrency;
        private BigDecimal localCostAmount;
        private BigDecimal taxAmount;
        private BigDecimal overseasTax;
        private String localCostCurrency;
        private BigDecimal overseasCostAmount;
        private String overseasCostCurrency;
        private BigDecimal totalOverseasAmount;
        private BigDecimal taxPercentage;
        private String taxAmountCurrency;
        private String taxCode;
        private String taxMasterCode;
        private boolean taxMasterOverride;
        private BigDecimal totalAmount;
        private String totalAmountCurrency;
        private boolean noTax;
        private String jobNumber;// may not be required as we are storing at bill level
        private String invoiceNumber;
        private String supplierInvoiceReference;
        private String placeOfSupply;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime invoiceDate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime dueDate;
        private Boolean isDueDateAutoPopulated;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime documentRecordDate;
        private String comments;
        private BigDecimal reportingAmount;
        private String costAccount;
        private String reference;
        private String bankAccount;
        private String chequeNumber;
        private String chequeBook;
        private String payTypes;
        private BigDecimal costCurrencyExchangeUpdate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime taxDate;
        private String tdsMasterId;
        private BigDecimal tdsAmount;
        private BigDecimal overseasTdsAmount;
        private BigDecimal tdsPercentage;
        private Boolean isRcm;
        private String stateName;
        private String stateCode;
        private String stateId;

        @JsonIgnore
        private String measurementContainerTypeId;

        private String advancePaymentNumber;

        private String advancePaymentId;
        private BigDecimal advancePaymentBalance;
        private Boolean isTaxPercentageOverride;
        private Boolean isOverrideSystemTax;
        private String billChargesStatus;
    }

}
