package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryV3Response;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Data;

@Data
@ApiModel(value = "Packing response model")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PackingListResponse implements IRunnerResponse {
    private List<PackingResponse> packings = new ArrayList<>();
    private Long assignedPackageCount = 0L;
    private Long unassignedPackageCount = 0L;
    private PackSummaryV3Response packSummary;
    @JsonIgnore
    private Integer totalPages = 0;
    @JsonIgnore
    private Long totalCount = 0L;
    private Map<String, Object> masterData;
}

