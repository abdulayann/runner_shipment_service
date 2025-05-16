package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class ShipmentWtVolResponse implements IRunnerResponse {

    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weight;
    private String weightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volume;
    private String volumeUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargeable;
    private String chargeableUnit;
    private Integer packs;
    private String packsType;
    private Integer assignedContainerCount;
    private BigDecimal assignedContainerTeu;
}
