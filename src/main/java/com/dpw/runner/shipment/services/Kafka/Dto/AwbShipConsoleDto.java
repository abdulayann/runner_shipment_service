package com.dpw.runner.shipment.services.Kafka.Dto;

import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
public class AwbShipConsoleDto implements Serializable {
    private String shipmentId;
    private String consolidationNumber;
    private LocalDateTime createdAt;
    private Integer tenantId;
    private String updatedBy;
}
