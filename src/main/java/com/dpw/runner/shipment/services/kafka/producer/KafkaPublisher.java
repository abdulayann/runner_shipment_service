package com.dpw.runner.shipment.services.kafka.producer;

import com.dpw.runner.shipment.services.entity.InternalEvent;
import com.dpw.runner.shipment.services.executors.PostCommitActionExecutor;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaPayload;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;

import java.time.LocalDateTime;

public abstract class KafkaPublisher<T extends KafkaPayload> implements IKafkaPublisher<T> {

    @Autowired
    protected KafkaTemplate<String, Object> kafkaTemplate;

    @Autowired
    protected PostCommitActionExecutor postCommitExecutor;

    @Autowired
    private InternalEventRepository internalEventRepository;

    @Autowired
    private JsonHelper jsonHelper;

    protected abstract String getTopic();

    @Override
    public void publish(T payload, String transactionId, Long entityId, String entityType) {
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
        postCommitExecutor.executeAfterCommit(getTopic(), payload, transactionId, eventId, kafkaTemplate);
    }
}
