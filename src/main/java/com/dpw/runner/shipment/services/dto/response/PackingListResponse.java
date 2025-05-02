package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
@ApiModel(value = "Packing response model")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PackingListResponse implements IRunnerResponse {
    private List<PackingResponse> packings = new ArrayList<>();
    private Long assignedPackageCount = 0L;
    private Long unassignedPackageCount = 0L;
    @JsonIgnore
    private Integer totalPages = 0;
    @JsonIgnore
    private Long totalCount = 0L;
}

