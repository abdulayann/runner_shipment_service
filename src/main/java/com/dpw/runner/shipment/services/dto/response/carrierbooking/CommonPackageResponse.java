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
public class CommonPackageResponse {

    private Long id;   // from MultiTenancy / BaseEntity

    private String containerNo;
    private Integer packs;
    private String packsUnit;
    private String hsCode;
    private String commodityDescription;
    private String goodsDescription;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Long shippingInstructionId;

}
