package com.dpw.runner.booking.services.dto.v1.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class V1DataResponse implements IRunnerResponse {
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
