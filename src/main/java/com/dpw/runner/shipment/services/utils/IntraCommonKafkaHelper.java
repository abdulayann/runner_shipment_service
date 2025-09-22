package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.entity.enums.GenericKafkaMsgType;
import com.dpw.runner.shipment.services.entity.enums.IntraKafkaOperationType;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.kafka.dto.GenericKafkaPayload;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class IntraCommonKafkaHelper {

    @Autowired
    private KafkaProducer producer;

    @Value("${bridge.inttra.messages.kafka.producer.topic}")
    String topic;

    public void sendDataToKafka(String payload, GenericKafkaMsgType msgType, IntraKafkaOperationType operationType) {
        try {
            // Wrap it inside your generic payload with the provided msgType
            GenericKafkaPayload genericKafkaMsg =
                    new GenericKafkaPayload(msgType, payload, operationType);

            log.debug("Payload sent to kafka with id {} and type {}", genericKafkaMsg.getId(), msgType);

            producer.produceToKafka(genericKafkaMsg, topic, genericKafkaMsg.getId());

        } catch (Exception e) {
            throw new GenericException("Error serializing payload for Kafka", e);
        }
    }
}
