package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsDataDto;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@Generated
public class DpsConsumer {
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IDpsEventService dpsEventService;

    @KafkaListener(topics = {"#{'${dps.kafka.topic-name}'}"}, groupId = "#{'${dps.kafka.group-id}'}")
    public void consume(String message) {
        try {
            log.info("{} | Received event message: {}", LoggerEvent.KAFKA_DPS, message);

            // Deserialize the incoming message into DpsDto
            DpsDto dpsDto = objectMapper.readValue(message, DpsDto.class);

            // Additional validation to ensure payload is not null
            if (dpsDto != null && dpsDto.getData() != null) {
                DpsDataDto payload = dpsDto.getData();

                // Check if the entity type is SHIPMENT
                if ("SHIPMENT".equalsIgnoreCase(payload.getEntityType())) {
                    log.info("{} | Processing SHIPMENT entity for ruleExecutionId: {}", LoggerEvent.KAFKA_DPS, payload.getRuleExecutionId());

                    // Save the DPS event
                    dpsEventService.saveDpsEvent(dpsDto);
                    log.info("{} | SHIPMENT entity processed successfully", LoggerEvent.KAFKA_DPS);
                } else {
                    log.warn("{} | Ignored entity type: {}", LoggerEvent.KAFKA_DPS, payload.getEntityType());
                }
            } else {
                log.error("{} | Invalid DPS event: Payload is null or invalid", LoggerEvent.KAFKA_DPS);
            }
        } catch (Exception ex) {
            log.error("{} | DPS ERROR: Exception occurred while processing message: {} with exception: {}", LoggerEvent.KAFKA_DPS, message, ex.getMessage(), ex);
        }
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
