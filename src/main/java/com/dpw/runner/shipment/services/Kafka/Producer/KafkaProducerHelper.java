package com.dpw.runner.shipment.services.Kafka.Producer;

import com.dpw.runner.shipment.services.entity.RetryMessageEntity;
import com.dpw.runner.shipment.services.entity.enums.EventMessageStatusEnum;
import com.dpw.runner.shipment.services.entity.enums.MessageTypeEnum;
import com.dpw.runner.shipment.services.repository.interfaces.RetryMessageRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@Slf4j
public class KafkaProducerHelper {

    @Autowired
    private KafkaProducer kafkaProducer;

    @Autowired
    private RetryMessageRepository retryMessageRepository;

    @Autowired
    private ObjectMapper objectMapper;

    public <T> void produceToKafka(T payload, String messageKey, Map<String, String> headers, String topic, RetryMessageEntity retryMessageEntity) {
        try {
            kafkaProducer.produceToKafka(payload, messageKey, headers, topic);
            if (retryMessageEntity != null) {
                retryMessageEntity.setStatus(EventMessageStatusEnum.SUCCESS);
                retryMessageRepository.save(retryMessageEntity);
            }
        } catch (Exception exception) {
            log.error("Pushing Retry Event Message for transactionId " + messageKey);
            if (retryMessageEntity != null) {
                retryMessageEntity.setRetryCount(retryMessageEntity.getRetryCount() + 1);
                retryMessageEntity.setStatus(EventMessageStatusEnum.FAILED);
                retryMessageRepository.save(retryMessageEntity);
                return;
            }

            recordFailedEvent(payload, messageKey, headers, topic);
        }
    }

    private <T> void recordFailedEvent(T payload, String messageKey, Map<String, String> headers, String topic) {
        RetryMessageEntity retryEntity = new RetryMessageEntity();
        retryEntity.setMessageType(MessageTypeEnum.OUTBOUND);
        try {
            retryEntity.setPayload(objectMapper.writeValueAsString(payload));
        } catch (JsonProcessingException e) {
            return;
        }
        retryEntity.setRetryCount(1);
        retryEntity.setTopic(topic);
        retryEntity.setMessageKey(messageKey);
        retryEntity.setHeaders(headers);
        retryEntity.setStatus(EventMessageStatusEnum.FAILED);

        retryMessageRepository.save(retryEntity);
    }
}
