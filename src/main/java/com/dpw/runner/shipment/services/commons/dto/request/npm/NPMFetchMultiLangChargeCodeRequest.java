package com.dpw.runner.shipment.services.commons.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class NPMFetchMultiLangChargeCodeRequest implements IRunnerRequest {
    @JsonProperty("key")
    public String key;
    @JsonProperty("lang")
    public String lang;
    @JsonProperty("key_type")
    public String key_type;
}
