package com.dpw.runner.shipment.services.kafka.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DpsDto {

    private UUID executionId;
    private String entityId;
    private String workflowType;    // DpsWorkflowType
    private String state;           // DpsWorkflowState
    private String status;          // DpsExecutionStatus
    private String entityType;      // DpsEntityType
    private List<String> fieldsDetected;
    private List<String> fieldsDetectedValues;
    private String text;
    private List<String> implications;
    private List<String> conditionMessage;
}
