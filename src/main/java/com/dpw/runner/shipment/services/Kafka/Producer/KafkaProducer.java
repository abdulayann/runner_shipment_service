package com.dpw.runner.shipment.services.Kafka.Producer;

import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.Map;

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
            // todo;: do this in your service, let this be generic
            // todo;: add entry in FO table
            log.info("Data produced to Kafka successfully");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public <T> void produceToKafka(T payload, String transactionId, Map<String, String> headerMap, String topic) {
        log.info("Processing result to kafka queue - " + topic + " with transactionId " + transactionId);
        try {
            log.info("request payload to kafka: " + objectMapper.writeValueAsString(payload) + " with transactionId "+ transactionId);
        } catch (JsonProcessingException e) {
            log.error("Error while posting to shipment, unable to convert object to json due to: "+ e.getMessage() + " with transactionId "+ transactionId);
        }
        try {
            Message<?> message = MessageBuilder
                    .withPayload(this.objectMapper.writeValueAsString(payload))
                    .setHeader("kafka_topic", topic)
                    .setHeader("kafka_messageKey", transactionId)
                    .copyHeaders(headerMap)
                    .build();
            kafkaTemplate.send(message);
            // todo;: do this in your service, let this be generic
            // todo;: add entry in FO table
            log.info("Data produced to Kafka successfully with transactionId "+ transactionId);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
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

