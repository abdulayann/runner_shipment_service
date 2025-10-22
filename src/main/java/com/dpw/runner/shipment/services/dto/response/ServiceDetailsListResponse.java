package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
@Schema(description = "Service Details List response model")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ServiceDetailsListResponse implements IRunnerResponse {
    private List<ServiceDetailsResponse> serviceDetailsResponses = new ArrayList<>();
    @JsonIgnore
    private Integer totalPages = 0;
    @JsonIgnore
    private Long totalCount = 0L;
    private Map<String, Object> masterData;
}

