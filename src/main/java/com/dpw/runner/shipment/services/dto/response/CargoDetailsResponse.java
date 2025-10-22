package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Schema(description = "Cargo Details Response Model")
public class CargoDetailsResponse {
    private String transportMode;
    private String shipmentType;
    private Integer noOfPacks;
    private String packsUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volume;
    private String volumeUnit;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargable;
    private String chargeableUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weight;
    private String weightUnit;
    private Integer containers;
    private BigDecimal teuCount;
    private Integer dgPacks;
    private String dgPacksUnit;
    private Boolean isVolumeEditable = Boolean.FALSE;
    private Boolean isCargoSummaryEditable = Boolean.FALSE;
    private Boolean isWeightEditable = Boolean.FALSE;
    private ShipmentSummaryWarningsResponse shipmentSummaryWarningsResponse;
    private BigDecimal containerVolume;
}
