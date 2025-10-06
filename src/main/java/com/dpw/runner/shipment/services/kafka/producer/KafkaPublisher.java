package com.dpw.runner.shipment.services.kafka.producer;

import com.dpw.runner.shipment.services.entity.InternalEvent;
import com.dpw.runner.shipment.services.executors.PostCommitActionExecutor;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaPayload;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDateTime;

@Log4j2
public abstract class KafkaPublisher<T extends KafkaPayload> implements IKafkaPublisher<T> {

    @Autowired
    protected PostCommitActionExecutor postCommitExecutor;

    @Autowired
    private InternalEventRepository internalEventRepository;

    @Autowired
    private JsonHelper jsonHelper;

    protected abstract String getTopic();

    @Override
    public void publish(T payload, String transactionId, Long entityId, String entityType) {
        log.info("Publishing event. TransactionId={}, EntityId={}, EntityType={}, Topic={}",
                transactionId, entityId, entityType, getTopic());
        //Save to internal_events table
        InternalEvent event = internalEventRepository.save(
                InternalEvent.builder()
                        .entityId(entityId)
                        .entityType(entityType)
                        .payload(jsonHelper.convertToJson(payload))
                        .publishedStatus("Publish_Pending")
                        .publishedTimestamp(LocalDateTime.now())
                        .consumedStatus("Consume_Pending")
                        .consumedTimestamp(LocalDateTime.now())
                        .build()
        );

        Long eventId = event.getId();
        log.debug("Event persisted in DB with EventId={}, TransactionId={}", eventId, transactionId);
        postCommitExecutor.executeAfterCommit(getTopic(), payload, transactionId, eventId);
        log.info("Post-commit execution scheduled successfully. EventId={}, Topic={}", eventId, getTopic());
    }
}

