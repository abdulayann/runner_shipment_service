package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.time.LocalDateTime;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

class PostCommitActionExecutorTest {

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private InternalEventRepository internalEventRepository;

    @Mock
    private KafkaTemplate<String, Object> kafkaTemplate;

    @InjectMocks
    private PostCommitActionExecutor executor;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        // Ensure synchronization is active
        if (!TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.initSynchronization();
        }
    }

    @Test
    void testExecuteAfterCommit_successfulPublish() {
        // Arrange
        String topic = "test-topic";
        String payload = "test-payload";
        String transactionId = "tx123";
        Long eventId = 123L;
        String payloadJson = "{\"data\":\"test\"}";

        when(jsonHelper.convertToJson(payload)).thenReturn(payloadJson);

        // Act
        executor.executeAfterCommit(topic, payload, transactionId, eventId, kafkaTemplate);

        // Trigger afterCommit manually
        for (TransactionSynchronization sync : TransactionSynchronizationManager.getSynchronizations()) {
            sync.afterCommit();
        }

        // Assert
        verify(kafkaTemplate).send(topic, transactionId, payloadJson);
        verify(internalEventRepository).updatePublishedStatus(eq(eventId), eq("Published"), any(LocalDateTime.class));
    }

    @Test
    void testExecuteAfterCommit_failedPublish() {
        // Arrange
        String topic = "test-topic";
        String payload = "test-payload";
        String transactionId = "tx123";
        Long eventId = 123L;

        when(jsonHelper.convertToJson(payload)).thenThrow(new RuntimeException("Serialization error"));

        // Act
        executor.executeAfterCommit(topic, payload, transactionId, eventId, kafkaTemplate);

        // Trigger afterCommit manually
        for (TransactionSynchronization sync : TransactionSynchronizationManager.getSynchronizations()) {
            sync.afterCommit();
        }

        // Assert
        verify(kafkaTemplate, never()).send(anyString(), anyString(), any());
        verify(internalEventRepository).updatePublishedStatus(eq(eventId), eq("Publish_Failed"), any(LocalDateTime.class));
    }
}
