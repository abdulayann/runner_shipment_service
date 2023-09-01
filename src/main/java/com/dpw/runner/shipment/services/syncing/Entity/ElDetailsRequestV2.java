package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class ElDetailsRequestV2 {
    private Long ShipmentId; //int64
    private String ElNumber;
    private Integer Packages; // int32
    private String Unit;
    private Decimal Weight;
    private String WeightUnit;
    private Integer MergeClass; // int32
    private Integer MergePackages; // int32
    private String MergePackageUnit;
    private Boolean Partition;
    private Integer PartitionSeqNumber; // int32
}
