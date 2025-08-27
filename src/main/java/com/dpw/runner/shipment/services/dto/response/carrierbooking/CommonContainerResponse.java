package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CommonContainerResponse {

    private Long id;   // from MultiTenancy / BaseEntity

    private String containerCode;
    private Integer count;
    private String goodsDescription;
    private String hsCode;
    private String commodityCode;
    private BigDecimal grossWeight;
    private BigDecimal volume;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private String grossWeightUnit;
    private String volumeUnit;
    private String containerNo;
    private Integer packs;
    private String packsUnit;
    private BigDecimal tareWeight;
    private String tareWeightUnit;
    private String sealNumber;
    private Long carrierBookingId;
    private Long shippingInstructionId;
}
