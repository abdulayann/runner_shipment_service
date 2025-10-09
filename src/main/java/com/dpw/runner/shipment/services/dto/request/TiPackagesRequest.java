package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
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
    @Size(max=5, message = "max size is 5 for noOfPackages")
    private String noOfPackages;
    @NotBlank(message = "Package Type is required")
    private String packageType;
    @Size(max=1024, message = "max size is 1024 for description")
    private String description;
    private String dimensions;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean dangerous;
    @Size(max=1024, message = "max size is 1024 for substanceName")
    private String substanceName;
    @Size(max=10, message = "max size is 10 for unNumber")
    private String unNumber;
    private String hazardLabel;
    @Size(max=10, message = "max size is 10 for tunnelRestrictionCode")
    private String tunnelRestrictionCode;
}
