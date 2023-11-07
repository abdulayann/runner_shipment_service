package com.dpw.runner.shipment.services.Kafka.Producer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class KafkaProducer {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private KafkaTemplate<String, Object> kafkaTemplate;

    public <T> void produceToKafka(T payload, String senderQueue, String transactionId) {
        log.info("Processing result to kafka queue - " + senderQueue);
        try {
            log.info("request payload to kafka: " + objectMapper.writeValueAsString(payload));
        } catch (JsonProcessingException e) {
            log.error("Error while converting shipment, unable to convert object to json due to: "+ e.getMessage());
        }
        try {
            kafkaTemplate.send(senderQueue, transactionId, payload);
            // todo;: do this in your service, let this be generic
            // todo;: add entry in FO table
            log.info("Data produced to Kafka successfully");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}

