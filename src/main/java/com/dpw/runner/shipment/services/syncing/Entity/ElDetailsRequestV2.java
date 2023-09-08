package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class ElDetailsRequestV2 {
    @JsonProperty("ShipmentId")
    private Long ShipmentId; //int64
    @JsonProperty("ElNumber")
    private String ElNumber;
    @JsonProperty("Packages")
    private Integer Packages; // int32
    @JsonProperty("Unit")
    private String Unit;
    @JsonProperty("Weight")
    private BigDecimal Weight;
    @JsonProperty("WeightUnit")
    private String WeightUnit;
    @JsonProperty("MergeClass")
    private Integer MergeClass; // int32
    @JsonProperty("MergePackages")
    private Integer MergePackages; // int32
    @JsonProperty("MergePackageUnit")
    private String MergePackageUnit;
    @JsonProperty("Partition")
    private Boolean Partition;
    @JsonProperty("PartitionSeqNumber")
    private Integer PartitionSeqNumber; // int32
}
