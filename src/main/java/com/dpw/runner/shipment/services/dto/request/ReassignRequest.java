package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Request for Reassign Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ReassignRequest implements IRunnerRequest {
    private Long id;
    private int branchId;
    private String remarks;
}
