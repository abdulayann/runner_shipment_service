package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class TransportInstructionLegsListResponse implements IRunnerResponse {
    private List<TransportInstructionLegsResponse> tiLegsResponses = new ArrayList<>();
    @JsonIgnore
    private Integer totalPages = 0;
    @JsonIgnore
    private Long totalCount = 0L;
    private Map<String, Object> masterData;
}
