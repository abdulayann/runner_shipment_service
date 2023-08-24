package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class NPMFetchOffersRequestFromUI extends CommonRequest implements IRunnerRequest {
    private String origin;
    private String destination;
    private Long bookingId;
    private String preferredDate;
    private String preferredDateType;
    private List<Container> containers;
    private List<Pack> packs;
    private String modeOfTransport;
    private String cargoType;
    private String serviceMode;
    private ContractsInfo contractsInfo;
    private boolean fetchDefaultRates;
    private String carrierCode;
    private String pol;
    private String pod;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Container {
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
    public static class Pack {
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
    public static class ContractsInfo {
        private String customerOrgId;
        private String contractId;
    }
}
