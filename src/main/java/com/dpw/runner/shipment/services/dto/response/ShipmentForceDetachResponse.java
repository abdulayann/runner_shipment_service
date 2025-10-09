package com.dpw.runner.shipment.services.dto.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.math.BigDecimal;

@Data
@Builder
@Schema("Shipment Detach Response")
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
