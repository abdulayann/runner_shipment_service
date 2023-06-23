package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.MergeClass;
import io.swagger.annotations.ApiModel;
import lombok.Builder;
import lombok.Data;
import lombok.ToString;

import java.util.UUID;

@Data
@Builder
@ApiModel("EL Details Response Model")
@ToString
public class ELDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private String elNumber;
    private Long packages;
    private String unit;
    private Long weight;
    private String weightUnit;
    private MergeClass mergeClass;
    private Long mergePackages;
    private String mergePackageUnit;
    private Boolean partition;
    private Long partitionSeqNumber;
}