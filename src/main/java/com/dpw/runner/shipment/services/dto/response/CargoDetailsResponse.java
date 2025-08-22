package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import io.swagger.annotations.ApiModel;
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
@ApiModel("Cargo Details Response Model")
public class CargoDetailsResponse {
    private String transportMode;
    private String shipmentType;
    private Integer noOfPacks;
    private String packsUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal chargable;
    private String chargeableUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
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
