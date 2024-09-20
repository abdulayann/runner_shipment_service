package com.dpw.runner.shipment.services.dto.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IataTactRatesApiResponse implements Serializable {
    @JsonProperty("Rates")
    @JsonFormat(with = JsonFormat.Feature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
    private List<Rates> rates;
    @JsonProperty("ResponseType")
    private String responseType;
    @JsonProperty("Errors")
    @JsonFormat(with = JsonFormat.Feature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
    private List<Errors> errors;

    @Builder
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Rates implements Serializable {
        @JsonProperty("EffectiveDate")
        private LocalDateTime effectiveDate;
        @JsonProperty("RateType")
        private String rateType;
        @JsonProperty("Carrier")
        private String carrier;
        @JsonProperty("Origin")
        private String origin;
        @JsonProperty("Destination")
        private String destination;
        @JsonProperty("CurrencyCode")
        private String currencyCode;
        @JsonProperty("StandardCharge")
        private StandardCharge standardCharge;

    }

    @Builder
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class StandardCharge implements Serializable {
        @JsonProperty("MinimumCharge")
        private BigDecimal minimumCharge;
        @JsonProperty("NormalCharge")
        private BigDecimal normalCharge;
        @JsonProperty("WeightCharge")
        private BigDecimal weightCharge;
        @JsonProperty("WeightBreak")
        @JsonFormat(with = JsonFormat.Feature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
        private List<WeightBreak> weightBreak;
    }

    @Builder
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class WeightBreak implements Serializable {
        @JsonProperty("WeightMeasure")
        private BigDecimal weightMeasure;
        @JsonProperty("Charge")
        private BigDecimal charge;
    }

    @Builder
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Errors implements Serializable {
        @JsonProperty("ParameterName")
        private String parameterName;
        @JsonProperty("ErrorType")
        private String errorType;
        @JsonProperty("Message")
        private String message;
    }
}
