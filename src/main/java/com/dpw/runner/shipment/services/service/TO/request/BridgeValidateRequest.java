package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@AllArgsConstructor
@Setter
@NoArgsConstructor
@Builder
public class BridgeValidateRequest {

    @JsonProperty("templateType")
    private String templateType;

    @JsonProperty("templateId")
    private String templateId;

    @JsonProperty("template")
    private String template;

    @JsonProperty("payload")
    private String payload;
}
