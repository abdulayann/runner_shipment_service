package com.dpw.runner.shipment.services.dto.request.oceanDG;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.ApprovalStatus;
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
  private ApprovalStatus status;
  private String remarks;
  private String taskId;
  private String requesterUserEmailId;
}
