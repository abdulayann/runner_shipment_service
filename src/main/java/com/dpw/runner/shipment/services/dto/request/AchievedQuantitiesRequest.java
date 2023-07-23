package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;
import org.apache.poi.hpsf.Decimal;

import javax.persistence.Column;
import java.math.BigDecimal;
import java.time.LocalDateTime;
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
}