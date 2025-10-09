package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@Schema(description = "Notes Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class NotesResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String text;
    private Long entityId;
    private String entityType;
    private String createdBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
    private String insertUserDisplayName;
    private Boolean isPublic;
    private Boolean isActive;
    private String label;
    private String assignedTo;
    private LocalDateTime updatedAt;
    private String updatedBy;
    private Boolean isReadOnly;
}
