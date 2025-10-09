package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Setter
@Schema("Container Response Model")
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ContainerListResponse implements IRunnerResponse {

    private List<ContainerBaseResponse> containers;
    private Map<String, Object> masterData;
    private Long assignedContainerCount = 0L;
    private Long unassignedContainerCount = 0L;
    private Boolean triggerMigrationWarning;
    @JsonIgnore
    private Integer totalPages;
    @JsonIgnore
    private Long numberOfRecords;
}
