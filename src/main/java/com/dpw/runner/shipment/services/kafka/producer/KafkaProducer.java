package com.dpw.runner.shipment.services.kafka.producer;

import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@Generated
public class KafkaProducer {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private KafkaTemplate<String, Object> kafkaTemplate;

    @Async
    public <T> void produceToKafka(T payload, String senderQueue, String transactionId) {
        log.info("Processing result to kafka queue - " + senderQueue);
        try {
            log.info("request payload to kafka: with transactionId {} and payload {}" , transactionId,  objectMapper.writeValueAsString(payload));
        } catch (JsonProcessingException e) {
            log.error("Error while converting data, unable to convert object to json due to: "+ e.getMessage());
        }
        try {
            kafkaTemplate.send(senderQueue, transactionId, payload);
            // LATER;: do this in your service, let this be generic
            // LATER;: add entry in FO table
            log.info("Data produced to Kafka successfully");
        } catch (Exception e) {
            throw new GenericException(e);
        }
    }

    public KafkaResponse getKafkaResponse(Object data, boolean isCreate) {
        KafkaResponse kafkaResponse = new KafkaResponse();
        if(isCreate) {
            kafkaResponse.setEvent(Constants.KAFKA_EVENT_CREATE);
        }
        else {
            kafkaResponse.setEvent(Constants.KAFKA_EVENT_UPDATE);
        }
        kafkaResponse.setData(data);
        return kafkaResponse;
    }
}

