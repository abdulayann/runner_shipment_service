package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class NPMFetchOffersRequestFromUI extends CommonRequest implements IRunnerRequest {
    private String origin;
    private String destination;
    private String countryCode;
    @JsonProperty("booking_id")
    private Long bookingId;
    @JsonProperty("preferred_date")
    private String preferredDate;
    @JsonProperty("preferred_date_type")
    private String preferredDateType;
    private List<Container> containers;
    private List<Pack> packs;
    @JsonProperty("mode_of_transport")
    private String modeOfTransport;
    @JsonProperty("cargo_type")
    private String cargoType;
    @JsonProperty("service_mode")
    private String serviceMode;
    @JsonProperty("contracts_info")
    private ContractsInfo contractsInfo;
    @JsonProperty("fetch_default_rates")
    private boolean fetchDefaultRates;
    @JsonProperty("carrier_code")
    private String carrierCode;
    private String pol;
    private String pod;
    private String direction;
    private BigDecimal weight;
    private String weight_uom;
    private BigDecimal volume;
    private String volume_uom;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Container implements Serializable {
        private Long id;
        private String containerType;
        private String commodityCode;
        private Long quantity;
        private BigDecimal grossWeight;
        private String grossWeightUnit;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Pack implements Serializable {
        private Long id;
        private Long quantity;
        private BigDecimal length;
        private BigDecimal width;
        private BigDecimal height;
        private String uom;
        private BigDecimal chargeable;
        private String weightUnit;
        private String volumeUnit;
        private BigDecimal volume;
        private BigDecimal weight;
        private String chargeableUnit;
        private String commodity;
        private boolean isHazardous;
        private String goodsDescription;
        private String packageType;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ContractsInfo implements Serializable {
        @JsonProperty("customer_org_id")
        private String customerOrgId;
        @JsonProperty("contract_id")
        private String contractId;
    }
}
