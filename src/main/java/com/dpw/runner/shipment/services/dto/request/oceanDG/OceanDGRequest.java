package com.dpw.runner.shipment.services.dto.request.oceanDG;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OceanDGRequest implements IRunnerRequest {
  private Integer shipmentId;
  private String status; //Enum--
  private String remarks;
  private String taskId;
  private String requesterUserEmailId;
  //roleId -> Commercial / DG
}
