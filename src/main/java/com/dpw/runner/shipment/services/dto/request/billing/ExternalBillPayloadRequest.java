package com.dpw.runner.shipment.services.dto.request.billing;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InvoiceBigDecimal2JsonDeserializer;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import org.springframework.stereotype.Component;

@Data
@Component @Generated
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

        private String jobStatus;
        private Boolean isHiplHrBilling;
        private String moduleType;
        private String moduleGuid;
        @NotEmpty(message = "ClientId can not be empty")
        private String clientId;
        @NotEmpty(message = "ClientAddressId can not be empty")
        private String clientAddressId;
        private Boolean actualInvoiceDateEnabled;
        private Boolean manualInvoiceDateEnabled;
        private Boolean isLocked;
        private String lastLoadJson;
    }

    @Data
    @Builder
    public static class ExternalBillChargeRequest implements IRunnerRequest {

        private boolean postARInvoice;
        private boolean postAPInvoice;
        private BillChargesRequest billChargeRequest;
    }

    @Data
    @Builder
    public static class BillChargesRequest implements IRunnerRequest {

        private String chargeTypeCode;
        private Boolean isFromConsolidation = false;
        private List<String> containerGuids;
        @Valid
        private BillChargeCostDetailsRequest billChargeCostDetails;
        @Valid
        private BillChargeRevenueDetailsRequest billChargeRevenueDetails;
        private List<String> ignoreValidations;
        private List<String> autoCalculate;
        private String payableLocation;
        private String rateSource = "MANUAL";
    }

    @Data
    @Builder
    public static class BillChargeCostDetailsRequest implements IRunnerRequest {

        private String creditorId;
        private String creditorAddressId;
        private String measurementBasis;
        private BigDecimal measurementBasisQuantity;
        private String measurementBasisUnit;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal unitRate;
        private String unitRateCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal localCostAmount;
        private String localCostCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasCostAmount;
        private String overseasCostCurrency;
        private Boolean noTax = false;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime invoiceDate;
        @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
        private LocalDateTime documentRecordDate;
        private Boolean isRcm = false;
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
        private BigDecimal unitRate;
        private String unitRateCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal localSellAmount;
        private String localSellCurrency;
        @JsonDeserialize(using = InvoiceBigDecimal2JsonDeserializer.class)
        private BigDecimal overseasSellAmount;
        private String overseasSellCurrency;
        private Boolean noTax = false;
        private Boolean isRcm = false;
        private String internalRemarks;
        private String externalRemarks;
    }

    @Data
    @Builder
    public static class ExternalBillConfiguration implements IRunnerRequest {

        private List<String> autoCalculate;
        private List<String> ignoreValidations;
    }

}
