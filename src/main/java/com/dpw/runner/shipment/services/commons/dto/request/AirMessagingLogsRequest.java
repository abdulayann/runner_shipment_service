package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AirMessagingLogsRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private UUID entityGuid;
    private String errorMessage;
    private String messageType;
    private String xmlPayload;
    private String status;
}
