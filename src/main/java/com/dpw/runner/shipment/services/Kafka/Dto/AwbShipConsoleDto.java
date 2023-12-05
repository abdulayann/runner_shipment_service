package com.dpw.runner.shipment.services.Kafka.Dto;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class AwbShipConsoleDto {
    private String shipmentId;
    private String consolidationNumber;
    private LocalDateTime createdAt;
    private Integer tenantId;
    private String updatedBy;
}
