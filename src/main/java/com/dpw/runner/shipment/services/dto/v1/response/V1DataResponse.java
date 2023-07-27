package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class V1DataResponse {
    @JsonProperty("Entities")
    public Object entities;
    @JsonProperty("Values")
    public Object values;
    @JsonProperty("TotalCount")
    public int totalCount;
    @JsonProperty("Skip")
    public int skip;
    @JsonProperty("Take")
    public int take;
    @JsonProperty("Error")
    public Object error;
    @JsonProperty("EntityId")
    public Long entityId;
}
