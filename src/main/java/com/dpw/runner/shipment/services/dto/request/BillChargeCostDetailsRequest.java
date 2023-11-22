package com.dpw.runner.shipment.services.dto.request;


import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
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
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BillChargeCostDetailsRequest implements Serializable {
    @NotBlank(message = "creditor Id can not be empty")
    private String creditorId;
    @NotBlank(message = "creditor Address Id can not be empty")
    private String creditorAddressId;
    private MeasurementBasis measurementBasis;//Weight
    private BigDecimal measurementBasisQuantity;
    private String measurementBasisUnit;//Kg
    private String measurementContainerTypeCode;
    private BigDecimal estimatedAmount;
    private String estimatedAmountCurrency;

    private BigDecimal unitRate;
    private String unitRateCurrency;
    @NotNull(message = "Local Cost Amount can not be null or empty")

    private BigDecimal localCostAmount;
    @NotBlank(message = "Local Cost Currency can not be null or empty")
    private String localCostCurrency;
    @NotNull(message = "Overseas Cost Amount can not be null or empty")

    private BigDecimal overseasCostAmount;
    @NotBlank(message = "Overseas Cost Currency can not be null or empty")
    private String overseasCostCurrency;

    private BigDecimal overseasTax;

    private BigDecimal taxPercentage;
    private String taxCode;

    private BigDecimal taxAmount;

    private BigDecimal totalAmount;
    private String totalAmountCurrency;
    private boolean isGSTApplicable;
    private String jobNumber;// may not be required as we are storing at bill level
    private String invoiceNumber;

    private LocalDateTime invoiceDate;

    private LocalDateTime dueDate;

    private LocalDateTime documentRecordDate;
    private String comments;

    private BigDecimal reportExchangeAmount;
    private String reportExchangeCurrency;
    private String costAccount;
    private String tdsMasterId;
    private BigDecimal tdsAmount;
    private BigDecimal tdsPercentage;
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
}
