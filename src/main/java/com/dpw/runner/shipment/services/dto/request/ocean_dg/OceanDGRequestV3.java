package com.dpw.runner.shipment.services.dto.request.ocean_dg;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OceanDGRequestV3 implements IRunnerRequest {
  private Long shipmentId;
  private String shipmentGuid;
  private TaskStatus status;
  private String remarks;
  private List<String> taskGuids;
  private String userEmail;
}
