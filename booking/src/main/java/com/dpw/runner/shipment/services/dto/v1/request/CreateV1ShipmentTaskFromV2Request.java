package com.dpw.runner.shipment.services.dto.v1.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CreateV1ShipmentTaskFromV2Request {
    @JsonProperty("shipmentId")
    private String shipmentId;
    @JsonProperty("sendToBranch")
    private List<Integer> sendToBranch;
    @JsonProperty("sendToOrg")
    private List<String> sendToOrg;
    @JsonProperty("additionalDocs")
    private List<String> additionalDocs;
}
