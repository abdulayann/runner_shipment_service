package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AwbAirMessagingResponse extends AwbResponse implements IRunnerResponse {
    private Meta meta;
    @Data
    @Builder
    public static class Meta implements Serializable {
        private Map<String, String> chargeDue;
        private Map<String, String> chargeBasis;
        private Map<String, String> rateClass;
        private Map<String, String> schCodes;
        private OrgDetails shipper;
        private OrgDetails consignee;
        private OrgDetails issueingAgent;
        private UnlocDetails pol;
        private UnlocDetails pod;
        private List<AwbRoutingInfoRes> awbRoutingInfo;
        private TenantInfo tenantInfo;
        private BigDecimal totalAmount;
        private String customOriginCode;
        private UserInfo userInfo;
        private String masterAwbNumber;
        private String entityNumber;
        private BigDecimal masterGrossWeightSum;
        private String masterGrossWeightSumUnit;
        private Integer masterPackCount;
        private String executedAtCity;
        private Integer currencyDecimalPlaces;
        private Integer weightDecimalPlaces;
        private Integer volumeDecimalPlaces;
        private Boolean includeCSD;
        private String additionalSecurityInfo;
    }

    @Data
    @Builder
    public static class OrgDetails implements Serializable {
        private String city;
        private String country;
        private String currency;
        private String number;
        private String postCode;
        private LocalDateTime expiry;
        private Boolean isRA;
        private Boolean isKC;
    }
    @Data
    @Builder
    public static class UnlocDetails implements Serializable {
        private String name;
        private String locCode;
        private String countyCode;
        private String iataCode;
    }

    @Data
    @Builder
    public static class TenantInfo implements Serializable {
        private String pimaAddress;
        private String country;
        private String city;
        private String state;
        private String number;
        private LocalDateTime expiry;
        private String branchName;
        private String branchCode;
        private String legalEntityName;
        private Boolean isShipmentV3Enabled;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AwbRoutingInfoRes extends AwbRoutingInfoResponse {
        private String originIATACode;
        private String destinationIATACode;
        private String originPortUnlocName;
        private String destinationPortUnlocName;
        private AirlineInfo airlineInfo;
    }
    @Data
    @Builder
    public static class AirlineInfo implements Serializable {
        private String iata;
        private String airlinePrefix;
    }

    @Data
    @Builder
    public static class UserInfo implements Serializable {
        private String userName;
        private String userDisplayName;
    }
}
