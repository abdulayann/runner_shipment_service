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
public class BillingBulkSummaryRequest implements IRunnerRequest {
    private String moduleType;
    private List<String> moduleGuids;
}
