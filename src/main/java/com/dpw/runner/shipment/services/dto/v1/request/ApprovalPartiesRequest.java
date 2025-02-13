package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ApprovalPartiesRequest implements IRunnerRequest {

    private List<ApprovalParty> creditDetailsRequests;
    private String operation;

    @Builder
    @Data
    public static class ApprovalParty implements IRunnerRequest {
        String addressId;
        String entityId;
        String entityType;
        String orgId;
        String orgType;
    }
}
