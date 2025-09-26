package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.patchrequest.ContainerV3PatchRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainerV3PatchBulkUpdateRequest implements IRunnerRequest {
    private List<Long> containerIds;
    private ContainerV3PatchRequest containerV3PatchRequest;
}
