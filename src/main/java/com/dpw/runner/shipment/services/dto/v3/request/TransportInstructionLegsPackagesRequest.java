package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.validator.annotations.MaxTotalDigits;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsPackagesRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    @NotNull(message = "Transport Instruction leg Id is required")
    private Long tiLegId;
    @Size(max = 5, message = "max size is 5 for noOfPackages")
    private String noOfPackages;
    @NotBlank(message = "Package Type is required")
    private String packageType;
    @Size(max = 1024, message = "max size is 1024 for description")
    private String description;
    @MaxTotalDigits(5)
    private BigDecimal length;
    private String lengthUnit;
    @MaxTotalDigits(5)
    private BigDecimal width;
    private String widthUnit;
    @MaxTotalDigits(5)
    private BigDecimal height;
    private String heightUnit;
    @MaxTotalDigits(15)
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    @MaxTotalDigits(15)
    private BigDecimal netWeight;
    private String netWeightUnit;
    @MaxTotalDigits(10)
    private BigDecimal volume;
    private String volumeUnit;
    private Boolean dangerous;
    @Size(max = 1024, message = "max size is 1024 for substanceName")
    private String substanceName;
    @Size(max = 10, message = "max size is 10 for unNumber")
    private String unNumber;
    private String hazardLabel;
    @Size(max = 10, message = "max size is 10 for tunnelRestrictionCode")
    private String tunnelRestrictionCode;
}
