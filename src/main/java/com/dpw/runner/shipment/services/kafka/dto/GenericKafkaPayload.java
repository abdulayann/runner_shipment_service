package com.dpw.runner.shipment.services.kafka.dto;

import com.dpw.runner.shipment.services.entity.enums.GenericKafkaMsgType;
import com.dpw.runner.shipment.services.entity.enums.IntraKafkaOperationType;
import lombok.Getter;

import java.util.UUID;

@Getter
public class GenericKafkaPayload {
    private final String id;      // unique id generated
    private final GenericKafkaMsgType type;    // entity type
    private final String payload; // serialized JSON payload
    private final IntraKafkaOperationType operationType;

    public GenericKafkaPayload(GenericKafkaMsgType type, String payload, IntraKafkaOperationType operationType) {
        this.id = UUID.randomUUID().toString();
        this.type = type;
        this.payload = payload;
        this.operationType = operationType;
    }
}
