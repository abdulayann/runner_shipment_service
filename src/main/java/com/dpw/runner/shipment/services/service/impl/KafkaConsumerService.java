package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.service.IKafkaConsumerService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
public class KafkaConsumerService implements IKafkaConsumerService {

    private final Logger LOG = LoggerFactory.getLogger(KafkaConsumerService.class);

    @Override
    @KafkaListener(topics = "sample_topic-1", groupId = "default")
    public void consume(String message) {
        LOG.info("Received message : {}", message);
    }
}
