package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModel;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Data
@Setter
@ApiModel("Container Response Model")
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ContainerListResponse implements IRunnerResponse {
    private List<ContainerBaseResponse> containers;
    private Map<String, Object> masterData;
    private Long assignedContainerCount = 0L;
    private Long unassignedContainerCount = 0L;
    @JsonIgnore
    private Integer totalPages;
    @JsonIgnore
    private Long numberOfRecords;
}
