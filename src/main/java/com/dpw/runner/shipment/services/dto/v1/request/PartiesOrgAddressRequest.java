package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PartiesOrgAddressRequest {
  @JsonProperty("Party")
  PartiesRequest party;
  @JsonProperty("OrgCode")
  private String OrgCode;
  @JsonProperty("AddressCode")
  private String AddressCode;

}
