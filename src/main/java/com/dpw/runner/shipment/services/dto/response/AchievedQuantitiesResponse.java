package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;
import org.apache.poi.hpsf.Decimal;

import java.time.LocalDateTime;
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
    private Decimal weightVolume;
    private String weightVolumeUnit;
    private Decimal consolidatedWeight;
    private String consolidatedWeightUnit;
    private Decimal consolidatedVolume;
    private String consolidatedVolumeUnit;
    private Decimal consolidationChargeQuantity;
    private String consolidationChargeQuantityUnit;
}
