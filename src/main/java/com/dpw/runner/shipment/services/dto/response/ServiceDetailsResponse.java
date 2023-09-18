package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class ServiceDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private Long consolidationId;
    private String serviceType;
    private PartiesResponse contractor;
    private int srvLocation;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime bookingDate;
    private Long serviceCount;
    private Duration serviceDuration;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime completionDate;
    private String refNumber;
    private String serviceNotes;
}
