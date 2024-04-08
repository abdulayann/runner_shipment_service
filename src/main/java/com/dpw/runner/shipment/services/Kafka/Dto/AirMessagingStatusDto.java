package com.dpw.runner.shipment.services.Kafka.Dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AirMessagingStatusDto {
    private String messageType;
    private String xmlPayload;
    private String errorMessage;
    private UUID guid;
    private String status;
}
