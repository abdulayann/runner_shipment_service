package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;


@Data
@Builder
@ApiModel("Achieved Quantities Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class AchievedQuantitiesRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private BigDecimal consolidatedWeight;
    private String consolidatedWeightUnit;
    private BigDecimal consolidatedVolume;
    private String consolidatedVolumeUnit;
    private BigDecimal consolidationChargeQuantity;
    private String consolidationChargeQuantityUnit;
    private String weightUtilization;
    private String volumeUtilization;
    private Integer packs;
    private String packsType;
    private Integer containerCount;
    private BigDecimal teuCount;
}