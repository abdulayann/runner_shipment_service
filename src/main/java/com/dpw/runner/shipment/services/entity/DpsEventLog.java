package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import lombok.*;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
public class DpsEventLog extends BaseEntity {
    private String executionId;
    private String transactionId;
    private String usernameList;
    private DpsWorkflowType workflowType;
    private DpsWorkflowState dpsWorkflowState;
    private DpsExecutionStatus status;
    private String ruleMatchedFieldList;
    private String implicationList;
    private String approvalDetailList;
    private LocalDateTime eventTimeStamp;
    private String shipmentId;
    private Integer tenantId;
}
