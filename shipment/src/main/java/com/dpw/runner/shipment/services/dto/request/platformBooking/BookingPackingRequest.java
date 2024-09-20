package com.dpw.runner.shipment.services.dto.request.platformBooking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BookingPackingRequest extends CommonRequest implements IRunnerRequest {
    private String reference_id;
    @JsonProperty("count")
    private Integer packs;
    @JsonProperty("packType")
    private String packsType;
    private BigDecimal length;
    private BigDecimal width;
    private BigDecimal height;
    private String dimensionUnit;
    private BigDecimal volume;
    private String volumeUnit;
    @JsonProperty("grossWeight")
    private BigDecimal weight;
    @JsonProperty("grossWeightUnit")
    private String weightUnit;
    private BigDecimal chargeable;
    private String chargeableUnit;
    @JsonProperty("commodity")
    private String commodityGroup;
    @JsonProperty("hsCode")
    private String HSCode;
    @JsonProperty("isHazardous")
    private Boolean hazardous;
    private String goodsDescription;
    // Internals
    private String lengthUnit;
    private String widthUnit;
    private String heightUnit;
    @JsonProperty("isDimension")
    private Boolean isDimension;
}
