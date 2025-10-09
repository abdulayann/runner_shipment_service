package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.validator.annotations.MaxTotalDigits;
import com.dpw.runner.shipment.services.validator.annotations.ValidPackageRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
@ValidPackageRequest
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
    @Size(max = 63, message = "max size is 63 for proper shipping name")
    private String properShippingName;
    @Size(max = 31, message = "max size is 31 for packing group")
    private String packingGroup;
    private BigDecimal minimumFlashPoint;
    @Size(max = 3, message = "max size is 3 for minimum flash point unit")
    private String minimumFlashPointUnit;
    private Boolean marinePollutant;
    @Size(max = 255, message = "max size is 255 for dg class description")
    private String dgClassDescription;
    private String transportMode;
}
