package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@ApiModel("Notes Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class NotesResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String text;
    private Long entityId;
    private String entityType;
    private String insertUserId;
    private LocalDateTime insertDate;
    private String insertUserDisplayName;
    private Boolean isPublic;
    private Boolean isActive;
}
