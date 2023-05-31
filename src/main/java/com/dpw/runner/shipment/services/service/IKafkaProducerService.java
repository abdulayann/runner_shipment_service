package com.dpw.runner.shipment.services.service;

public interface IKafkaProducerService {

    public void sendMessage(String message);
}
