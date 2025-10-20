package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@Component
@Log4j2
public class PostCommitActionExecutor {

    private static final String STATUS_PUBLISHED = "Published";
    private static final String STATUS_PUBLISH_FAILED = "Publish_Failed";

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private InternalEventRepository internalEventRepository;

    public <T> void executeAfterCommit(String topic, T payload, String transactionId,
                                       Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {

        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            registerPostCommitAction(topic, payload, transactionId, eventId, kafkaTemplate);
            log.debug("Transaction synchronization registered for EventId={}", eventId);
        } else {
            log.warn("No active transaction found. Publishing immediately. EventId={}, TransactionId={}", eventId, transactionId);
            publishEventWithStatusUpdate(topic, payload, transactionId, eventId, kafkaTemplate);
        }
    }

    private <T> void registerPostCommitAction(String topic, T payload, String transactionId,
                                              Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
            @Override
            public void afterCommit() {
                publishEventWithStatusUpdate(topic, payload, transactionId, eventId, kafkaTemplate);
            }
        });
    }

    private <T> void publishEventWithStatusUpdate(String topic, T payload, String transactionId,
                                                  Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {
        try {
            String messageJson = prepareMessageJson(eventId, payload);
            kafkaTemplate.send(topic, transactionId, messageJson);
            internalEventRepository.updatePublishedStatus(eventId, STATUS_PUBLISHED, LocalDateTime.now());
            logEventSuccess(eventId, transactionId, topic, messageJson);
        } catch (Exception e) {
            handlePublishFailure(eventId, transactionId, topic, e);
        }
    }

    private <T> String prepareMessageJson(Long eventId, T payload) {
        Map<String, Object> messageWrapper = new HashMap<>();
        messageWrapper.put("eventId", eventId);
        messageWrapper.put("payload", payload);
        return jsonHelper.convertToJson(messageWrapper);
    }

    private void handlePublishFailure(Long eventId, String transactionId, String topic, Exception e) {
        try {
            internalEventRepository.updatePublishedStatus(eventId, STATUS_PUBLISH_FAILED, LocalDateTime.now());
        } catch (Exception updateEx) {
            log.error("Failed to update event status after publish failure. EventId={}, Error={}",
                    eventId, updateEx.getMessage(), updateEx);
        }
        log.error("Failed to publish Kafka event. EventId={}, TransactionId={}, Topic={}, Error={}",
                eventId, transactionId, topic, e.getMessage(), e);
    }

    private void logEventSuccess(Long eventId, String transactionId, String topic, String messageJson) {
        int payloadSize = Optional.ofNullable(messageJson)
                .map(msg -> msg.getBytes(StandardCharsets.UTF_8).length)
                .orElse(0);
        log.info("Kafka event published successfully. EventId={}, TransactionId={}, Topic={}, PayloadSize={} bytes",
                eventId, transactionId, topic, payloadSize);
    }
}
