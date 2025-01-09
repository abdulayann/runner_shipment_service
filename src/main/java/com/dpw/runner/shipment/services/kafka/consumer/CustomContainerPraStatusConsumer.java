package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.CustomContainerDto;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Instant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
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
public class CustomContainerPraStatusConsumer {

    private final IConsolidationRepository consolidationRepository;
    private final IContainerRepository containerRepository;
    private final ObjectMapper objectMapper;
    private final RetryTemplate retryTemplate;

    @Autowired
    public CustomContainerPraStatusConsumer(IConsolidationRepository consolidationRepository,
            IContainerRepository containerRepository, ObjectMapper objectMapper, RetryTemplate retryTemplate) {
        this.consolidationRepository = consolidationRepository;
        this.containerRepository = containerRepository;
        this.objectMapper = objectMapper;
        this.retryTemplate = retryTemplate;
    }

    // @KafkaListener(
    //         groupId = "#{'${custom.service.container.event.kafka.subs}'}",
    //         topics = "#{'${custom.service.container.event.kafka.queue}'}",
    //         autoStartup = "#{'${custom.service.container.event.kafka.autostart}'}",
    //         containerFactory = "customServiceContainerEventKafkaListenerContainerFactory")
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
                log.info("{} | Custom event message: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, message);
                CustomContainerDto obj = objectMapper.readValue(message, CustomContainerDto.class);
                if (ObjectUtils.isNotEmpty(obj) && ObjectUtils.isNotEmpty(obj.getPayload())) {
                    Long consolidationId = consolidationRepository.findIdByGuid(obj.getPayload().getConsolidationGuid());
                    if (ObjectUtils.isNotEmpty(consolidationId)) {
                        containerRepository.savePraStatus(obj.getPayload().getCustomsStatus().toString(), obj.getPayload().getContainerGuid(), consolidationId);
                        log.info("Passed");
                    } else {
                        log.info("Consolidation Data is not present for event: {} for message: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, message);
                    }
                } else {
                    log.info("Empty event from Customer Service for event: {} for message: {} ", LoggerEvent.CUSTOM_SERVICE_EVENT, message);
                }
                return null;
            });
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, message, ex.getLocalizedMessage());
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
        log.info("{} Received message from topic: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, topic);
        log.info("{} Partition: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, partition);
        log.info("{} Offset: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, kafkaMessage);
    }
}
