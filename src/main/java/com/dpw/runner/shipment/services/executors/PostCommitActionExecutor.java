package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;

@Component
public class PostCommitActionExecutor {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private KafkaProducer producer;

    public <T> void executeAfterCommit(Supplier<T> entitySupplier,
            Function<T, KafkaResponse> kafkaMapper, String guid, String queueName) {
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
            @Override
            public void afterCommit() {
                T entity = entitySupplier.get();
                if(entity != null) {
                    KafkaResponse response = kafkaMapper.apply(entity);
                    response.setTransactionId(UUID.randomUUID().toString());
                    producer.produceToKafka(
                            jsonHelper.convertToJson(response),
                            queueName,
                            guid
                    );
                }
            }
        });
    }
}
