package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TriggerSyncRequest implements IRunnerRequest {
    private String moduleType;
    private List<Integer> tenantIds;
}
