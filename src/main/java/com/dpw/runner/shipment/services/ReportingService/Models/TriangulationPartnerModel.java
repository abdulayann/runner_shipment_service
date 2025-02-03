package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TriangulationPartnerModel implements IDocumentModel {
    @JsonProperty("TriangulationPartner")
    private Long triangulationPartner;

    @JsonProperty("IsAccepted")
    private Boolean isAccepted;
}
