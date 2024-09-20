package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class HblCargoRequestV2 implements IRunnerRequest {
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
