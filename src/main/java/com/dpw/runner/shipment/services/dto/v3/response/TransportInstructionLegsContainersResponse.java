package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsContainersResponse implements IRunnerResponse {
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
