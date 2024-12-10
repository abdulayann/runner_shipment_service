package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsDataDto;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Instant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@Generated
public class DpsConsumer {

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IDpsEventService dpsEventService;
    @Autowired
    private RetryTemplate retryTemplate;

    @KafkaListener(
            groupId = "#{'${dps.kafka.group-id}'}",
            topics = "#{'${dps.kafka.topic-name}'}",
            autoStartup = "#{'${dps.kafka.consumer-auto-startup}'}",
            containerFactory = "dpsKafkaListenerContainerFactory")
    public void consume(
            @Payload String message,
            @Header(KafkaHeaders.RECEIVED_TOPIC) String topic,
            @Header(KafkaHeaders.RECEIVED_PARTITION_ID) int partition,
            @Header(KafkaHeaders.OFFSET) long offset,
            @Header(KafkaHeaders.RECEIVED_TIMESTAMP) long receivedTimestamp,
            Acknowledgment acknowledgment) {

        logKafkaMessageInfo(message, topic, partition, offset, receivedTimestamp);

        try {
            retryTemplate.execute(retryContext -> {

                // Deserialize the incoming message into DpsDto
                DpsDto dpsDto = objectMapper.readValue(message, DpsDto.class);

                // Additional validation to ensure payload is not null
                if (dpsDto != null && dpsDto.getData() != null) {
                    DpsDataDto payload = dpsDto.getData();

                    log.info("{} | Processing Dps message entity for rule execution id: {}", LoggerEvent.KAFKA_DPS, payload.getRuleExecutionId());
                    // Save the DPS event
                    dpsEventService.saveDpsEvent(dpsDto);
                } else {
                    log.error("{} | Invalid DPS event: Payload is null or invalid", LoggerEvent.KAFKA_DPS);
                }
                return null;
            });
        } catch (Exception ex) {
            log.error("{} | DPS ERROR: Exception occurred while processing message: {} with exception: {}", LoggerEvent.KAFKA_DPS, message, ex.getMessage(), ex);
        } finally {
            acknowledgment.acknowledge();
        }
    }

    private void logKafkaMessageInfo(
            String kafkaMessage,
            String topic,
            int partition,
            long offset,
            long receivedTimestamp) {
        log.info("{} Received message from topic: {}", LoggerEvent.KAFKA_DPS,topic);
        log.info("{} Partition: {}", LoggerEvent.KAFKA_DPS,partition);
        log.info("{} Offset: {}", LoggerEvent.KAFKA_DPS,offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.KAFKA_DPS,Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.KAFKA_DPS, kafkaMessage);
    }

    // SIMULATION DPS Events
//    @PostConstruct
//    private void simulateKafkaConsumption() {
//        try {
//            ObjectMapper objectMapper = new ObjectMapper();
//            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
//            objectMapper.registerModule(new JavaTimeModule());
//            // Assuming the JSON file is on the classpath, adjust if needed
//
//            DpsDto jsonPayload = objectMapper.readValue(new File("src/main/resources/dps_payload_sample.json"), DpsDto.class);
//            consume(objectMapper.writeValueAsString(jsonPayload)); // Simulate Kafka message consumption
//        } catch (Exception e) {
//            log.error("Error simulating Kafka message consumption: {}", e.getMessage());
//        }
//    }

}
