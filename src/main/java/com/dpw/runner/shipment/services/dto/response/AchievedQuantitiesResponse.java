package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ApiModel("Achieved Quantities Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AchievedQuantitiesResponse implements IRunnerResponse {
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
}
