package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

@Getter
@Setter
public class TILegsPackagesModel implements Serializable {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    @JsonProperty("TI_Packages")
    private String noOfPackages;
    @JsonProperty("TI_PackageType")
    private String packageType;
    @JsonProperty("TI_PackageDescription")
    private String description;
    @JsonProperty("TI_PackageDimensions")
    private String dimensions;
    @JsonProperty("TI_PackageGrossWeight")
    private BigDecimal grossWeight;
    @JsonProperty("TI_PackageGrossWeightUnit")
    private String grossWeightUnit;
    @JsonProperty("TI_PackageNetWeight")
    private BigDecimal netWeight;
    @JsonProperty("TI_PackageNetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("TI_PackageGrossVolume")
    private BigDecimal volume;
    @JsonProperty("TI_PackageGrossVolumeUnit")
    private String volumeUnit;
    @JsonProperty("TI_PackageDangerous")
    private Boolean dangerous;
    @JsonProperty("TI_PackageSubstance_Name")
    private String substanceName;
    @JsonProperty("TI_PackageUNNumber")
    private String unNumber;
    @JsonProperty("TI_PackageHazardLabelClass")
    private String hazardLabel;
    @JsonProperty("TI_PackageTunnelRestrictionCode")
    private String tunnelRestrictionCode;
}
