package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.MergeClass;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@Schema(description = "EL Details Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
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