package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.UUID;

@Getter
@Setter
public class TILegsReferenceModel implements Serializable {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    @JsonProperty("TI_ReferenceType")
    private String type;
    @JsonProperty("TI_Reference")
    private String reference;
}
