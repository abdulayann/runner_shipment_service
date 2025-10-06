package com.dpw.runner.shipment.services.service.interfaces;

public interface IKafkaEventPublisherService {
    <T> void publishToKafka(String topic, T payload, String transactionId, Long eventId);
}
