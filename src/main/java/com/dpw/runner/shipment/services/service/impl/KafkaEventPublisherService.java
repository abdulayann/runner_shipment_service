package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaEventPublisherService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;

import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Component
public class KafkaEventPublisherService implements IKafkaEventPublisherService {

    @Autowired
    protected KafkaTemplate<String, Object> kafkaTemplate;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private InternalEventRepository internalEventRepository;

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Override
    public <T> void publishToKafka(String topic, T payload, String transactionId, Long eventId) {
        try {
            Map<String, Object> messageWrapper = new HashMap<>();
            messageWrapper.put("eventId", eventId);
            messageWrapper.put("payload", payload);
            String messageJson = jsonHelper.convertToJson(messageWrapper);

            kafkaTemplate.send(topic, transactionId, messageJson);

            // Update status in a NEW transaction
            internalEventRepository.updatePublishedStatus(eventId, "Published", LocalDateTime.now());

            log.info("Kafka event published successfully. EventId={}, TransactionId={}, Topic={}, PayloadSize={} bytes",
                    eventId, transactionId, topic, messageJson != null ? messageJson.getBytes(StandardCharsets.UTF_8).length : 0);
        } catch (Exception e) {
            // On failed update status
            try {
                internalEventRepository.updatePublishedStatus(eventId, "Publish_Failed", LocalDateTime.now());
            } catch (Exception updateException) {
                log.error("Failed to update event status after publish failure. EventId={}, Error={}",
                        eventId, updateException.getMessage());
            }
            log.error("Failed to publish Kafka event. EventId={}, TransactionId={}, Topic={}, Error={}",
                    eventId, transactionId, topic, e.getMessage(), e);
        }
    }
}
