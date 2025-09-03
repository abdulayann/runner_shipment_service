package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiContainersRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    @NotBlank(message = "Container type is required")
    private String type;
    private String number;
    @Size(max=1024, message = "max size is 1024 for description")
    private String description;
    @Size(max=5, message = "max size is 5 for noOfPackages")
    private String noOfPackages;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean dangerous;
    @Size(max=1024, message = "max size is 1024 for SubstanceName")
    private String substanceName;
    @Size(max=10, message = "max size is 10 for unNumber")
    private String unNumber;
    private String dgClass;
    @Size(max=10, message = "max size is 10 for tunnel restriction code")
    private String tunnelRestrictionCode;
}
