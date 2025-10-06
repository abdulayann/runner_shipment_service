package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.service.interfaces.IKafkaEventPublisherService;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.concurrent.CompletableFuture;

@Component
@Log4j2
public class PostCommitActionExecutor {

    @Autowired
    private IKafkaEventPublisherService kafkaEventPublisherService;

    public <T> void executeAfterCommit(String topic, T payload, String transactionId, Long eventId) {

        // Check if we're in an active transaction
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            // We're in a transaction - register for post-commit execution
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    publishToKafkaAsync(topic, payload, transactionId, eventId);
                }
            });
            log.debug("Transaction synchronization registered for EventId={}", eventId);
        } else {
            // No active transaction - execute immediately in async manner
            log.warn("No active transaction found. Publishing to Kafka immediately. EventId={}, TransactionId={}", eventId, transactionId);
            publishToKafkaAsync(topic, payload, transactionId, eventId);
        }
    }

    private <T> void publishToKafkaAsync(String topic, T payload, String transactionId, Long eventId) {
        // Execute in async to avoid blocking
        CompletableFuture.runAsync(() ->
                kafkaEventPublisherService.publishToKafka(topic, payload, transactionId, eventId)
        ).exceptionally(ex -> {
            log.error("Async execution failed for Kafka publish. EventId={}, TransactionId={}", eventId, transactionId, ex);
            return null;
        });
    }
}