package com.dpw.runner.shipment.services.commons.dto.request.hbl;

import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;


@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblCargoDto implements Serializable {

    private Long id;
    private UUID guid;
    private Integer packageCount;
    private String packageType;
    private String classification;
    private String iMCONo;
    private Integer blContainerId;
    private String cargoGrossVolumeUnit;
    private BigDecimal cargoGrossVolume;
    private String cargoNetWeightUnit;
    private BigDecimal cargoNetWeight;
    private String cargoGrossWeightUnit;
    private BigDecimal cargoGrossWeight;
    private Boolean hazmatDetails;
    private String marksAndNumbers;
    private String cargoDesc;
    private String scheduleBNumber;
    private String hsCode;
    private String blContainerContainerNumber;
    private String cargoesCommodityDesc;
    private String unNo;


}