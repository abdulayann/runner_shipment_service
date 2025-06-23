package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@SuppressWarnings("java:S1948")
public class CheckCreditBalanceFusionResponse implements IRunnerResponse {
    @JsonProperty("Data")
    private FusionData data;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class FusionData implements Serializable {
        @JsonProperty("CallingSystem")
        private String callingSystem;
        @JsonProperty("BusinessUnit")
        private String businessUnit;
        @JsonProperty("SiteNumber")
        private String siteNumber;
        @JsonProperty("AccountNumber")
        private String accountNumber;
        @JsonProperty("CreditDetails")
        private List<CreditDetails> creditDetails;
        @JsonProperty("Message")
        private String message;

    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CreditDetails {
        @JsonProperty("Date")
        private String Date;
        @JsonProperty("BusinessUnit")
        private String BusinessUnit;
        @JsonProperty("SiteNumber")
        private String SiteNumber;
        @JsonProperty("AccountNumber")
        private String AccountNumber;
        @JsonProperty("OutstandingAmount")
        private double OutstandingAmount;
        @JsonProperty("FunctionalCurrencyCreditLimit")
        private double FunctionalCurrencyCreditLimit;
        @JsonProperty("CreditLimitINR")
        private double CreditLimitINR;
        @JsonProperty("CreditLimit")
        private double CreditLimit;
        @JsonProperty("CreditLimitCurrency")
        private String CreditLimitCurrency;
        @JsonProperty("Exchangerate")
        private double Exchangerate;
        @JsonProperty("SecurityDeposit")
        private double SecurityDeposit;
        @JsonProperty("TotalCreditLimit")
        private double TotalCreditLimit;
        @JsonProperty("PaymentTerms")
        private String PaymentTerms;
        @JsonProperty("TotalOutstanding")
        private double TotalOutstanding;
        @JsonProperty("NotDue")
        private double NotDue;
        @JsonProperty("OverDue")
        private double OverDue;
    }
}
