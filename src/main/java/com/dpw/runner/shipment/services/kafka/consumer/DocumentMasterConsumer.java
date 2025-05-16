package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.DocumentDto;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Instant;
import java.util.Objects;
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
public class DocumentMasterConsumer {

    private ObjectMapper objectMapper;

    @Autowired
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    private final RetryTemplate retryTemplate;

    @Autowired
    DocumentMasterConsumer(ObjectMapper objectMapper, RetryTemplate retryTemplate) {
        this.objectMapper = objectMapper;
        this.retryTemplate = retryTemplate;
    }

    @KafkaListener(
            topics = {"#{'${document.master.kafka.event}'}"},
            groupId = "#{'${document.master.kafka.subs}'}",
            autoStartup = "#{'${document.master.kafka.consumer-auto-startup}'}",
            containerFactory = "documentKafkaListenerContainerFactory")
    public void consume(@Payload String message,
            @Header(KafkaHeaders.RECEIVED_TOPIC) String topic,
            @Header(KafkaHeaders.RECEIVED_PARTITION_ID) int partition,
            @Header(KafkaHeaders.OFFSET) long offset,
            @Header(KafkaHeaders.RECEIVED_TIMESTAMP) long receivedTimestamp,
            Acknowledgment acknowledgment) {

        logKafkaMessageInfo(message, topic, partition, offset, receivedTimestamp);
        try {
            retryTemplate.execute(retryContext -> {
                log.info("{} | event message: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, message);
                DocumentDto object = objectMapper.readValue(StringEscapeUtils.unescapeJson(message), DocumentDto.class);

                if (Objects.nonNull(object) && Objects.nonNull(object.getData()) && Objects.nonNull(object.getAction())) {
                    bookingIntegrationsUtility.documentUploadEvent(object);
                }
                log.info("{} | Passed", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT);
                return null;
            });
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, message, ex.getLocalizedMessage());
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
        log.info("{} Received message from topic: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, topic);
        log.info("{} Partition: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, partition);
        log.info("{} Offset: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, kafkaMessage);
    }
}