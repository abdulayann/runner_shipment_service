package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.UUID;

@Getter
@Setter
public class TILegsContainersModel implements Serializable {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    @JsonProperty("TI_ContainerType")
    private String type;
    @JsonProperty("TI_ContainerNumber")
    private String number;
    @JsonProperty("TI_ContainerDescription")
    private String description;
    @JsonProperty("TI_ContainerPackages")
    private String noOfPackages;
    @JsonProperty("TI_GrossWeight")
    private String grossWeight;
    @JsonProperty("TI_GrossWeightUnit")
    private String grossWeightUnit;
    @JsonProperty("TI_NetWeight")
    private String netWeight;
    @JsonProperty("TI_NetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("TI_GrossVolume")
    private String volume;
    @JsonProperty("TI_GrossVolumeUnit")
    private String volumeUnit;
    @JsonProperty("TI_ContainerDangerous")
    private Boolean dangerous;
    @JsonProperty("TI_ContainerSubstance_Name")
    private String substanceName;
    @JsonProperty("TI_ContainerUNNumber")
    private String unNumber;
    @JsonProperty("TI_ContainerDGClass")
    private String dgClass;
    @JsonProperty("TI_ContainerTunnelRestrictionCode")
    private String tunnelRestrictionCode;
}
