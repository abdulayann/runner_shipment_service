package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;

@Data
@ApiModel("Notes Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotesRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private String text;
    private Long entityId;
    private String entityType;
    private String insertUserId;
    private LocalDateTime insertDate;
    private String insertUserDisplayName;
    private Boolean isPublic;
    private Boolean isActive;
    private String label;
    private String assignedTo;
    private String entityGuid;
}
