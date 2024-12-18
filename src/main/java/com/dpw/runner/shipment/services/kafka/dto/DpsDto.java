package com.dpw.runner.shipment.services.kafka.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

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
        @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSSSS")
        private LocalDateTime eventTimestamp;
        private List<Object> tasks;
        private String entityType;
        private List<String> conditionMessage;

    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DpsFieldDataDto {
        private String key;
        private String value;
    }

}
