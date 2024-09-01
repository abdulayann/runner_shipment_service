package com.dpw.runner.shipment.services.dto.v1.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class TaskResponse {
  Integer id;
  Integer usedId;
  String entityID;
  String taskType;
  String roleId;
  Integer tenantId;
  String userEmail;
  String userName;
  String status;
  String ShipmentId;
}
