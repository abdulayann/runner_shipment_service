package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.entity.Awb;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class NPMImportRatesResponse {
    @JsonProperty("updatedAwb")
    private Awb updatedAwb;
}
