package com.dpw.runner.shipment.services.kafka.producer;

public interface IKafkaPublisher<T> {
    void publish(T payload, String transactionId, Long eventId, String entityType);
}
