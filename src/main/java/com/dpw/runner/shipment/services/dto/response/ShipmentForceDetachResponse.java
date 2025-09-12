package com.dpw.runner.shipment.services.dto.response;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;

@Data
@Builder
@ApiModel("Shipment Detach Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentForceDetachResponse {

    private String shipmentNumber;
    private String containerNumber;
    private String packageCount;
    private String packageUnit;
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean packageAssigned;


}
