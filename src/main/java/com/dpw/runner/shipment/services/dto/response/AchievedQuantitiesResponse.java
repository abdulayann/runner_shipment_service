package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal consolidatedWeight;
    private String consolidatedWeightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal consolidatedVolume;
    private String consolidatedVolumeUnit;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal consolidationChargeQuantity;
    private String consolidationChargeQuantityUnit;
    private String weightUtilization;
    private String volumeUtilization;
    private Integer packs;
    private String packsType;
    private Integer dgPacks;
    private String dgPacksType;
    private Integer slacCount;
    private Integer dgContainerCount;
    private Integer containerCount;
    private BigDecimal teuCount;
}
