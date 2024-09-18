package com.dpw.runner.shipment.services.entity.fzb;


import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class EFZBPayload {

    @JsonProperty("MessageHeaderDocument")
    @Valid
    @NotNull(message = "messageHeaderDocument is required")
    private MessageHeaderDocumentFZB messageHeaderDocument;

    @JsonProperty("BusinessHeaderDocument")
    @Valid
    @NotNull(message = "businessHeaderDocument is required")
    private BusinessHeaderDocumentFZB businessHeaderDocument;

    @JsonProperty("MasterConsignment")
    @Valid
    @NotNull(message = "masterConsignment is required")
    private MasterConsignmentFZB masterConsignment;
}
