package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.stereotype.Component;

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
    private DpsWorkflowState dpsWorkflowState;
    private LocalDateTime eventTimeStamp;
    private String shipmentId;
    private Integer tenantId;
}
