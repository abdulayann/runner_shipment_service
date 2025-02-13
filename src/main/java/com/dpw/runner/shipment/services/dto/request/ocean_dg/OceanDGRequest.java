package com.dpw.runner.shipment.services.dto.request.ocean_dg;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OceanDGRequest implements IRunnerRequest {
    private Long shipmentId;
    private TaskStatus status;
    private String remarks;
    private String taskId;
    private String userEmail;
}
