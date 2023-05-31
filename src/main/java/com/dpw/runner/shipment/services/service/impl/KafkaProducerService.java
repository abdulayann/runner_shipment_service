package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.service.IKafkaProducerService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

@Service
public class KafkaProducerService implements IKafkaProducerService {

    private final Logger LOG = LoggerFactory.getLogger(KafkaProducerService.class);

    @Autowired
    private KafkaTemplate<String, String> kafkaTemplate;

    @Override
    public void sendMessage(String message) {
        LOG.info("Sending message to {}", "sample_topic");
        kafkaTemplate.send("sample_topic-1", message);
    }
}
