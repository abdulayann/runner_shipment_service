package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class NPMFetchLangChargeCodeResponse implements IRunnerResponse {
    @JsonProperty("key")
    private String key;
    @JsonProperty("language")
    private String language;
    @JsonProperty("translation")
    private String translation;
}
