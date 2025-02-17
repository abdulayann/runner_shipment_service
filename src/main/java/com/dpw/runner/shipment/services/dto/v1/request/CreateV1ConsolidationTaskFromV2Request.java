package com.dpw.runner.shipment.services.dto.v1.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CreateV1ConsolidationTaskFromV2Request {
    @JsonProperty("shipId")
    public List<String> shipId;
    @JsonProperty("docList")
    public List<List<String>> docList;
    @JsonProperty("consoleId")
    private String consoleId;
    @JsonProperty("sendToBranch")
    private List<Integer> sendToBranch;
    @JsonProperty("sendToOrg")
    private List<String> sendToOrg;
    @JsonProperty("additionalDocs")
    private List<String> additionalDocs;
}
