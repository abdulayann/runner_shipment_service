package com.dpw.runner.booking.services.Kafka.Dto;

import lombok.Data;

@Data
public class KafkaResponse {
    private String event;
    private Object data;
}
