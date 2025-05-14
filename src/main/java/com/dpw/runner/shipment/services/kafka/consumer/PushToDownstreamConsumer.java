package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.service.interfaces.IPushToDownstreamService;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.text.StringEscapeUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Service;

import java.time.Instant;


@Service
@Slf4j
@Generated
public class PushToDownstreamConsumer {

    private final ObjectMapper objectMapper;
    private final IPushToDownstreamService pushToDownstreamService;

    @Autowired
    PushToDownstreamConsumer(ObjectMapper objectMapper, IPushToDownstreamService pushToDownstreamService) {
        this.objectMapper = objectMapper;
        this.pushToDownstreamService = pushToDownstreamService;
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
            Acknowledgment acknowledgment) {

        logKafkaMessageInfo(message, topic, partition, offset, receivedTimestamp);
        try {
            PushToDownstreamEventDto object = objectMapper.readValue(StringEscapeUtils.unescapeJson(message), PushToDownstreamEventDto.class);
            pushToDownstreamService.process(object);

            log.info("{} | Passed", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM);
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, message, ex.getLocalizedMessage());
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
        log.info("{} Received message from topic: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, topic);
        log.info("{} Partition: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, partition);
        log.info("{} Offset: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.KAFKA_PUSH_TO_DOWNSTREAM, kafkaMessage);
    }
}