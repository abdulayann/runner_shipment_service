package com.dpw.runner.shipment.services.dto.v1.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class V1RoleIdRequest {
  Integer tenantId;
  String roleName;

}
