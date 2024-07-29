package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class AwbOtherInfoResponse implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String shipper;
    private String carrier;
    private String executedAt;
    private LocalDateTime executedOn;
}
