package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GetMatchingRulesRequest {
    private UUID shipmentGuid;
    private List<DpsExecutionStatus> dpsExecutionStatusList;
}
