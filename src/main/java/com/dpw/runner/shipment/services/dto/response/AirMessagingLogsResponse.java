package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AirMessagingLogsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private UUID entityGuid;
    private String errorMessage;
    private String messageType;
    private String xmlPayload;
    private String status;
    private LocalDateTime createdAt;
}
