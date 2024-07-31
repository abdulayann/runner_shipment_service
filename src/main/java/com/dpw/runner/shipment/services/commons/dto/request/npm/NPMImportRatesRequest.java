package com.dpw.runner.shipment.services.commons.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.entity.Awb;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class NPMImportRatesRequest implements IRunnerRequest {
    private Awb awb;
    @JsonProperty("EntityType")
    private String EntityType;
    private NPMOffer offer;
}
