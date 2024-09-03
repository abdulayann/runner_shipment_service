package com.dpw.runner.shipment.services.dto.request.oceanDG;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OceanDGApprovalRequest {
  private Long shipmentId;
  private String remarks;
}
