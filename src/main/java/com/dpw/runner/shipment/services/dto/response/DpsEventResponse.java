package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Generated
public class DpsEventResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private UUID executionId;
    private String entityId;
    private DpsEntityType entityType;
    private DpsWorkflowType workflowType;
    private DpsWorkflowState state;
    private DpsExecutionStatus status;
    private String text;
    private String matchingCondition;
    private List<String> implicationList;
    private List<String> conditionMessageList;
    private List<String> ruleMatchedFieldList;
    private transient List<DpsFieldDataResponse> dpsFieldData;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime updatedAt;
    private Boolean isDeleted;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime eventTimestamp;
    private String transactionId;
    private List<String> usernameList;
    private transient List<Object> tasks;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DpsFieldDataResponse {
        private String key;
        private String value;
    }
}
