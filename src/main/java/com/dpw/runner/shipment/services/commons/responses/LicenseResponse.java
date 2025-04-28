package com.dpw.runner.shipment.services.commons.responses;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import lombok.NoArgsConstructor;


@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LicenseResponse implements IRunnerResponse{
  private Long userId;
  private Integer companyId;
  private Integer tenantId;
  private String licenseName;
  private String licenseNumber;
  private Boolean hasValidLicense;

}
