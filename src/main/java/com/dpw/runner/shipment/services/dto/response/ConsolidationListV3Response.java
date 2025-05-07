package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationListV3Response implements IRunnerResponse {
    private List<ConsolidationListResponse> consolidationListResponses;
    @JsonIgnore
    private Integer totalPages;
    @JsonIgnore
    private Long numberOfRecords;
}
