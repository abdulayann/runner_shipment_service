package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @Builder
@AllArgsConstructor @NoArgsConstructor
public class SyncKafkaDto {
    private String data;
    private String guid;
    private String id;
    private String entity;
    private Integer tenantId;
    private String userName;
    private String transactionId;
    private String updateUsername;
}
