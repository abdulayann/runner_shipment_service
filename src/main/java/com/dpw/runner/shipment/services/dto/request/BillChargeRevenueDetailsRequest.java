package com.dpw.runner.shipment.services.dto.request;

import lombok.Getter;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Set;

@Getter
@Setter
public class BillChargeRevenueDetailsRequest implements Serializable {
    private String debtorId;
    private String debtorAddressId;
    private MeasurementBasis measurementBasis;//Weight
    private BigDecimal measurementBasisQuantity;
    private String measurementBasisUnit;//Kg
    private String measurementContainerTypeCode;
    private BigDecimal estimatedAmount;
    private String estimatedAmountCurrency;
    private BigDecimal unitRate;
    private String unitRateCurrency;

    @NotNull(message = "local Amount can not be null or empty")
   
    private BigDecimal localSellAmount;
    @NotBlank(message = "local Sell Currency can not be null or empty")
    private String localSellCurrency;
   
    private BigDecimal totalAmount;
   
    private BigDecimal taxPercentage;
   
    private BigDecimal taxAmount;
   
    @NotNull(message = "overseas Sell Amount can not be null or empty")
    private BigDecimal overseasSellAmount;
    @NotBlank(message = "overseas Sell Currency can not be null or empty")
    private String overseasSellCurrency;
   
    private BigDecimal overseasTax;
   
    private BigDecimal totalOverseasAmount;
    private String taxCode;
    @NotNull(message = "vendor total Amount can not be null or empty")
   
    private BigDecimal vendorTotalAmount;
    private String totalAmountCurrency;
    private boolean isGSTApplicable;
    private String taxInvoiceNumber;
    
    private LocalDateTime dueDate;
    
    private LocalDateTime documentRecordDate;
    private String comments;
    private String paymentTermsCode;
    private BigDecimal reportExchangeAmount;
    private String revenueAccount;
    private String customerPlaceOfSupply;
    
    private LocalDateTime taxDate;
    @Valid
    private Set<TaxDetailsRequest> taxDetails;
    @Valid
    private Set<SubTaxDetailsRequest> subTaxDetails;
    @Valid
    private Set<CurrencyExchangeRateDetailsRequest> currencyExchangeRateDetails;
    private Boolean isRcm = false;
}