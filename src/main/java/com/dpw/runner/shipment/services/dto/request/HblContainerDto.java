package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import lombok.*;

import java.math.BigDecimal;


@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblContainerDto extends CommonRequest {
    private Long id;
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
    private Integer  quantity;
    private String  quantityCode;

}