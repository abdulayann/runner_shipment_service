package com.dpw.runner.shipment.services.dto.v3.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BulkCloneLineItemRequest {
    @NotNull(message = "Module Id is required.")
    private Long moduleId;
    private Long containerId;
    private Long packageId;

    @NotNull(message = "Number of clones is required.")
    @Min(value = 1, message = "Number of clones must be at least 1.")
    @Max(value = 50, message = "Number of clones cannot exceed 50.")
    private Integer numberOfClones;
}
