package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.KafkaEventDTO;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IPushToDownstreamService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.time.Instant;
import java.time.LocalDateTime;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.text.StringEscapeUtils;
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
public class PushToDownstreamConsumer {

    private final ObjectMapper objectMapper;
    private final IPushToDownstreamService pushToDownstreamService;
    private final IV1Service v1Service;
    private final RetryTemplate retryTemplate;
    private final InternalEventRepository internalEventRepository;

    @Autowired
    PushToDownstreamConsumer(ObjectMapper objectMapper, IPushToDownstreamService pushToDownstreamService, IV1Service v1Service, RetryTemplate retryTemplate, InternalEventRepository internalEventRepository) {
        this.objectMapper = objectMapper;
        this.pushToDownstreamService = pushToDownstreamService;
        this.v1Service = v1Service;
        this.retryTemplate = retryTemplate;
        this.internalEventRepository = internalEventRepository;
    }

    @KafkaListener(
            topics = {"#{'${shipments.internal.messages.kafka}'}"},
            groupId = "#{'${shipments.internal.messages.kafka.consumer}'}",
            autoStartup = "#{'${push.to.downstream.kafka.consumer-auto-startup}'}",
            containerFactory = "documentKafkaListenerContainerFactory")
    public void consume(@Payload String message,
            @Header(KafkaHeaders.RECEIVED_TOPIC) String topic,
            @Header(KafkaHeaders.RECEIVED_PARTITION_ID) int partition,
            @Header(KafkaHeaders.OFFSET) long offset,
            @Header(KafkaHeaders.RECEIVED_TIMESTAMP) long receivedTimestamp,
            @Header(KafkaHeaders.RECEIVED_MESSAGE_KEY) String transactionId,
            Acknowledgment acknowledgment) {

        logKafkaMessageInfo(message, topic, partition, offset, receivedTimestamp, transactionId);

        Long eventId = null;
        try {
            JsonNode rootNode = objectMapper.readTree(StringEscapeUtils.unescapeJson(message));
            eventId = rootNode.get("eventId").asLong();
            String payloadJson = rootNode.get("payload").toString();

            PushToDownstreamEventDto object = parseMessage(payloadJson, topic, partition, offset);
            if (object == null) {
                return;
            }
            final Long finalEventId = eventId;
            retryTemplate.execute(retryContext -> {
                pushToDownstreamService.process(object, transactionId);
                internalEventRepository.updateConsumedStatus(finalEventId, "Consumed", LocalDateTime.now());
                log.info("{} | Passed", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM);
                return null;
            });

        } catch (Exception ex) {
            if (eventId != null) {
                internalEventRepository.updateConsumedStatus(eventId, "Consume_Failed", LocalDateTime.now());
            }
            log.error("Exception occurred for event: {} | message: {} | error: {}",
                    LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, message, ex.getMessage(), ex);
        } finally {
            v1Service.clearAuthContext();
            acknowledgment.acknowledge();
        }
    }

    /**
     * Parses Kafka message JSON into DTO. Returns null if parsing fails (and logs error).
     */
    private PushToDownstreamEventDto parseMessage(String message,
            String topic,
            int partition,
            long offset) {
        try {
            return objectMapper.readValue(
                    StringEscapeUtils.unescapeJson(message),
                    PushToDownstreamEventDto.class
            );
        } catch (IOException e) {
            log.error("Invalid JSON. Skipping retries. Event: {} | topic={} | partition={} | offset={} | message={}",
                    LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, topic, partition, offset, message, e);
            return null;
        }
    }

    private void logKafkaMessageInfo(
            String kafkaMessage,
            String topic,
            int partition,
            long offset,
            long receivedTimestamp,
            String transactionId) {
        log.info("{} Received message from topic: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, topic);
        log.info("{} Partition: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, partition);
        log.info("{} Offset: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, kafkaMessage);
        log.info("{} Transaction Id: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, transactionId);
    }
}