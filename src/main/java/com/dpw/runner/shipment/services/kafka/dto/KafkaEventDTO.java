package com.dpw.runner.shipment.services.kafka.dto;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KafkaEventDTO<T> {
    private Long eventId;
    private T payload;
}
