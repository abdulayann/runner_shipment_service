package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;

@Data
@Builder
@ApiModel("Shipment Detach Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentForceDetachResponse{

    private String shipmentNumber;
    private Long shipmentId;
    private String containerNumber;
    private Integer packageCount;
    private String packageUnit;
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean packageAssigned;


}
