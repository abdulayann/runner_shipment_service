package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiContainersResponse implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    private String type;
    private String number;
    private String description;
    private String noOfPackages;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean dangerous;
    private String substanceName;
    private String unNumber;
    private String dgClass;
    private String tunnelRestrictionCode;
}
