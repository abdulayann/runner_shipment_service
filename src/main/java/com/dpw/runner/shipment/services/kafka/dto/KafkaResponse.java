package com.dpw.runner.shipment.services.kafka.dto;

import lombok.Data;

@Data
public class KafkaResponse {
    private String event;
    private String transactionId;
    private Object data;
}
