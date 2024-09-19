package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EAWBPayload {
    @Valid
    @NotNull(message = "Message header document cannot be null")
    @JsonProperty("MessageHeaderDocument")
    private MessageHeaderDocument messageHeaderDocument;

    @Valid
    @NotNull(message = "Business header document cannot be null")
    @JsonProperty("BusinessHeaderDocument")
    private BusinessHeaderDocument businessHeaderDocument;

    @Valid
    @JsonProperty("MasterConsignment")
    @NotNull(message = "MasterConsignment cannot be null")
    private MasterConsignment masterConsignment;
}
