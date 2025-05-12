package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import lombok.Builder;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
@Builder
@ApiModel(value = "Routing response model")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RoutingListResponse implements IRunnerResponse {
    List<RoutingsResponse> routings = new ArrayList<>();
    @JsonIgnore
    private Integer totalPages = 0;
    @JsonIgnore
    private Long totalCount = 0L;
    private Map<String, Object> masterData;
}
