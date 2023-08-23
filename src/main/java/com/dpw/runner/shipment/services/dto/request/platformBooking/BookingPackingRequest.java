package com.dpw.runner.shipment.services.dto.request.platformBooking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.models.auth.In;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BookingPackingRequest extends CommonRequest implements IRunnerRequest {
    private UUID reference_id;
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
    private BigDecimal netWeight;
    @JsonProperty("grossWeightUnit")
    private String netWeightUnit;
    private BigDecimal chargeable;
    private String chargeableUnit;
    private String commodity;
    @JsonProperty("hsCode")
    private String HSCode;
    @JsonProperty("isHazardous")
    private Boolean hazardous;
    private String goodsDescription;
}
