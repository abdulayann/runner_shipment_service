package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiPackagesRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    private String noOfPackages;
    private String packageType;
    private String description;
    private String dimensions;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean dangerous;
    private String substanceName;
    private String unNumber;
    private String hazardLabel;
    private String tunnelRestrictionCode;
}
