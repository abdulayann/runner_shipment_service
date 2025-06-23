package com.dpw.runner.shipment.services.dto.request.hbl;

import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;


@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblContainerDto implements Serializable {
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
    private String  containerDesc;
    private Integer  dGClass;
    private Boolean  hazardousCheckBox;
    private Integer  hazardous;
    private String  hazardousMasterListDropDown;
    private String  hazardousUn;
    private String  containerTypeName;
    private String  packageUnit;
    private Long  quantity;
    private String  quantityCode;
    private String packsType;
    private Long containerCount;
}