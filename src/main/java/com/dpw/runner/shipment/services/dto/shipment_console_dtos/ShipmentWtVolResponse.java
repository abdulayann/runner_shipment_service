package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
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
    private BigDecimal chargable;
    private String chargeableUnit;
    private Integer packs;
    private String packsType;
    private Integer containerCount;
    private BigDecimal teuCount;
    private Integer dgContainerCount;
    private Integer dgPacks;
    private String dgPacksType;
    private Integer slacCount;
    private Long shipmentsCount;
    private Integer consoleContainerCount;
    private BigDecimal consoleTeuCount;
    private Integer consoleDgContainerCount;
    private Boolean isNonFtlOrFclAttached;
}
