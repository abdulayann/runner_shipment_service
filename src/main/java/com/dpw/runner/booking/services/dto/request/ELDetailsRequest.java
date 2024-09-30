package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.CommonRequest;
import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import com.dpw.runner.booking.services.entity.enums.MergeClass;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("ELDetails Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ELDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
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
