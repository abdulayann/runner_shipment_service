package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@Schema(description = "Integration Responses Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class IntegrationResponsesResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long entityId;
    private String entityType;
    private IntegrationType integrationType;
    private Status status;
    private String response_message;
    private String createdBy;
}
