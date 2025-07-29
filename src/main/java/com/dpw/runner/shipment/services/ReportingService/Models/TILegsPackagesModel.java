package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
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
    @JsonProperty("TI_PackageLength")
    private String length;
    @JsonProperty("TI_PackageLengthUnit")
    private String lengthUnit;
    @JsonProperty("TI_PackageWidth")
    private String width;
    @JsonProperty("TI_PackageWidthUnit")
    private String widthUnit;
    @JsonProperty("TI_PackageHeight")
    private String height;
    @JsonProperty("TI_PackageHeightUnit")
    private String heightUnit;
    @JsonProperty("TI_PackageGrossWeight")
    private String grossWeight;
    @JsonProperty("TI_PackageGrossWeightUnit")
    private String grossWeightUnit;
    @JsonProperty("TI_PackageNetWeight")
    private String netWeight;
    @JsonProperty("TI_PackageNetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("TI_PackageGrossVolume")
    private String volume;
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
