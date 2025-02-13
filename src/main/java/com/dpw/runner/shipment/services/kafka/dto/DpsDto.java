package com.dpw.runner.shipment.services.kafka.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class DpsDto {
    private String event;
    private DpsDataDto data;
    private long timestamp;
    private String transactionId;

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DpsDataDto {
        private UUID ruleExecutionId;
        private String actionExecutionId;
        private String entityId;
        private String workflowType;
        private String state;
        private String ruleStatus;
        @JsonProperty("fieldsDetected")
        private List<String> ruleMatchedFieldList;
        private String text;
        private String matchingCondition;
        private List<String> implications;
        @JsonProperty("username")
        private List<String> usernameList;
        private List<DpsFieldDataDto> fieldsDetectedValues;
        private LocalDateTime eventTimestamp;
        private List<Object> tasks;
        private String entityType;
        private List<String> conditionMessage;
        private List<DpsApprovalDetailDto> approvalLineUpdates;

    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DpsFieldDataDto {
        private String key;
        private String value;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DpsApprovalDetailDto {
        private String username;
        private LocalDateTime time;
        private String message;
        private String state;
        private String level;
        private String roleName;
        private String roleId;
    }

}
