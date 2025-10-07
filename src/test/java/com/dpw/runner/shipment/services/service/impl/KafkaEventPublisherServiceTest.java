package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.kafka.KafkaException;
import org.springframework.kafka.core.KafkaTemplate;

import java.time.LocalDateTime;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class KafkaEventPublisherServiceTest {
    @InjectMocks
    private KafkaEventPublisherService kafkaService;

    @Mock
    private KafkaTemplate<String, Object> kafkaTemplate;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private InternalEventRepository internalEventRepository;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testPublishToKafka_success() {
        String topic = "test-topic";
        String transactionId = "txn123";
        Long eventId = 1L;
        Object payload = "payload";

        String jsonPayload = "{\"eventId\":1,\"payload\":\"payload\"}";
        when(jsonHelper.convertToJson(any(Map.class))).thenReturn(jsonPayload);

        // Act
        kafkaService.publishToKafka(topic, payload, transactionId, eventId);

        // Verify Kafka send called
        verify(kafkaTemplate).send("test-topic", "txn123", eq(jsonPayload));

        // Verify DB updated to "Published"
        verify(internalEventRepository).updatePublishedStatus(eq(eventId), eq("Published"), any(LocalDateTime.class));
    }

    @Test
    void testPublishToKafka_kafkaFails() {
        String topic = "test-topic";
        String transactionId = "txn123";
        Long eventId = 1L;
        Object payload = "payload";

        String jsonPayload = "{\"eventId\":1,\"payload\":\"payload\"}";
        when(jsonHelper.convertToJson(any(Map.class))).thenReturn(jsonPayload);

        // Simulate Kafka failure
        doThrow(new KafkaException("Kafka failed")).when(kafkaTemplate).send(anyString(), anyString(), anyString());

        // Act
        kafkaService.publishToKafka(topic, payload, transactionId, eventId);

        // Verify DB updated to "Publish_Failed" after exception
        verify(internalEventRepository).updatePublishedStatus(eq(eventId), eq("Publish_Failed"), any(LocalDateTime.class));
    }

    @Test
    void testPublishToKafka_dbUpdateFailsAfterKafkaException() {
        String topic = "test-topic";
        String transactionId = "txn123";
        Long eventId = 1L;
        Object payload = "payload";

        String jsonPayload = "{\"eventId\":1,\"payload\":\"payload\"}";
        when(jsonHelper.convertToJson(any(Map.class))).thenReturn(jsonPayload);

        // Kafka fails
        doThrow(new KafkaException("Kafka failed")).when(kafkaTemplate).send(anyString(), anyString(), anyString());

        // DB fails while updating status
        doThrow(new RuntimeException("DB update failed")).when(internalEventRepository)
                .updatePublishedStatus(eq(eventId), eq("Publish_Failed"), any(LocalDateTime.class));

        // Act & Assert: method should not throw
        assertDoesNotThrow(() ->
                kafkaService.publishToKafka(topic, payload, transactionId, eventId)
        );

        // Verify DB update attempted
        verify(internalEventRepository).updatePublishedStatus(eq(eventId), eq("Publish_Failed"), any(LocalDateTime.class));
    }
}
