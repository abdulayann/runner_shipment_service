package com.dpw.runner.shipment.services.dto.v1.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class TaskRequest {
  private String entityType;
  private String entityID;
  private String roleId;
  private String status;
  private Integer userId;
  private Integer tenantId;
  private String taskType;
}
