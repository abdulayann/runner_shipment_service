package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class TenantFilterRequest implements IRunnerRequest {
    private Long orgId;
    private Long addressId;
}
