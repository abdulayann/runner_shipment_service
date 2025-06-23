package com.dpw.runner.shipment.services.dto.v1.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class RAKCDetailsResponse implements IRunnerResponse {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("OrgId")
    private Long orgId;
    @JsonProperty("OrgOrganizationCode")
    private String orgOrganizationCode;
    @JsonProperty("RegulatedAgent")
    private Boolean regulatedAgent = false;
    @JsonProperty("KnownConsignor")
    private Boolean knownConsignor = false;
    @JsonProperty("KCRANumber")
    private String kCRANumber;
    @JsonProperty("KCRAExpiry")
    private String kCRAExpiry;
}
