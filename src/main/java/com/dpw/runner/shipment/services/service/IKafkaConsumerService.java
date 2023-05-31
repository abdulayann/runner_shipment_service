package com.dpw.runner.shipment.services.service;

public interface IKafkaConsumerService {
    public void consume(String message);
}
