package com.dpw.runner.shipment.services.dto.request.billing;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;


@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BillingBulkSummaryBranchWiseRequest implements IRunnerRequest {
    private String moduleType;
    private List<ModuleData> moduleData;

    @Getter
    @Setter
    @ToString
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ModuleData implements IRunnerRequest {
        String branchId;
        String moduleGuid;
    }

}
