package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.entity.enums.MergeClass;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ELDetailsModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("ElNumber")
    private String elNumber;
    @JsonProperty("Packages")
    private Long packages;
    @JsonProperty("Unit")
    private String unit;
    @JsonProperty("Weight")
    private Long weight;
    @JsonProperty("WeightUnit")
    private String weightUnit;
    @JsonProperty("MergeClass")
    private MergeClass mergeClass;
    @JsonProperty("MergePackages")
    private Long mergePackages;
    @JsonProperty("MergePackageUnit")
    private String mergePackageUnit;
    @JsonProperty("Partition")
    private Boolean partition;
    @JsonProperty("PartitionSeqNumber")
    private Long partitionSeqNumber;
}
