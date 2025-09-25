package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.time.LocalDateTime;

@Component
@Log4j2
public class PostCommitActionExecutor {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private InternalEventRepository internalEventRepository;

    public <T> void executeAfterCommit(String topic, T payload, String transactionId, Long eventId, KafkaTemplate<String, Object> kafkaTemplate) {
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
            @Override
            public void afterCommit() {
                try {
                    String messageJson = jsonHelper.convertToJson(payload);
                    kafkaTemplate.send(topic, transactionId, jsonHelper.convertToJson(payload));
                    // On success update status
                    internalEventRepository.updatePublishedStatus(eventId, "Published", LocalDateTime.now());
                    log.info("Kafka event published successfully. EventId={}, TransactionId={}, Topic={}, PayloadSize={} bytes",
                            eventId, transactionId, topic, messageJson.length());
                } catch (Exception e) {
                    // On failed update status
                    internalEventRepository.updatePublishedStatus(eventId, "Publish_Failed", LocalDateTime.now());
                    log.error("Failed to publish Kafka event. EventId={}, TransactionId={}, Topic={}, Error={}",
                            eventId, transactionId, topic, e.getMessage(), e);
                }
            }
        });
    }
}

