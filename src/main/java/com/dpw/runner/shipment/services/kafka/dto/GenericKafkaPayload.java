package com.dpw.runner.shipment.services.kafka.dto;

import com.dpw.runner.shipment.services.entity.enums.GenericKafkaMsgType;
import lombok.Getter;

import java.util.UUID;

@Getter
public class GenericKafkaPayload {
    private final String id;      // unique id generated
    private final GenericKafkaMsgType type;    // entity type
    private final String payload; // serialized JSON payload

    public GenericKafkaPayload(GenericKafkaMsgType type, String payload) {
        this.id = UUID.randomUUID().toString();
        this.type = type;
        this.payload = payload;
    }
}
