package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Component
@Log4j2
public class PostCommitActionExecutor {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private InternalEventRepository internalEventRepository;

    public <T> void executeAfterCommit(String topic, T payload, String transactionId, Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {

        // Check if we're in an active transaction
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            // We're in a transaction - register for post-commit execution
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    publishToKafkaAsync(topic, payload, transactionId, eventId, kafkaTemplate);
                }
            });
            log.debug("Transaction synchronization registered for EventId={}", eventId);
        } else {
            // No active transaction - execute immediately in async manner
            log.warn("No active transaction found. Publishing to Kafka immediately. EventId={}, TransactionId={}", eventId, transactionId);
            publishToKafkaAsync(topic, payload, transactionId, eventId, kafkaTemplate);
        }
    }

    private <T> void publishToKafkaAsync(String topic, T payload, String transactionId, Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {
        // Execute in async to avoid blocking
        CompletableFuture.runAsync(() -> {
            publishToKafka(topic, payload, transactionId, eventId, kafkaTemplate);
        }).exceptionally(ex -> {
            log.error("Async execution failed for Kafka publish. EventId={}, TransactionId={}", eventId, transactionId, ex);
            return null;
        });
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    private <T> void publishToKafka(String topic, T payload, String transactionId, Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {
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