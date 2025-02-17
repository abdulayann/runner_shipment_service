package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class HblContainerRequestV2 implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private String containerNumber;
    private String containerType;
    private String carrierSealNumber;
    private String sealNumber;
    private Long noOfPackages;
    private String shipperSealNumber;
    private String customsSealNumber;
    private BigDecimal containerGrossWeight;
    private String containerGrossWeightUnit;
    private BigDecimal containerGrossVolume;
    private String containerGrossVolumeUnit;
    private String containerDesc;
    private Integer dGClass;
    private Boolean hazardousCheckBox;
    private Integer hazardous;
    private String hazardousMasterListDropDown;
    private String hazardousUn;
    private String containerTypeName;
    private String packageUnit;
    private Integer quantity;
    private String quantityCode;
}
