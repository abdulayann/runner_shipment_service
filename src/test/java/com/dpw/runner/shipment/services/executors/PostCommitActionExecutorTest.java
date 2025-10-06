package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.time.LocalDateTime;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
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
        String wrapperJson = "{\"eventId\":123,\"payload\":\"" + payloadJson + "\"}";

        when(jsonHelper.convertToJson(any(Map.class))).thenReturn(wrapperJson);

        // Act
        executor.executeAfterCommit(topic, payload, transactionId, eventId, kafkaTemplate);

        // Trigger afterCommit manually
        for (TransactionSynchronization sync : TransactionSynchronizationManager.getSynchronizations()) {
            sync.afterCommit();
        }

        // Assert
        verify(kafkaTemplate).send(topic, transactionId, wrapperJson);
        verify(internalEventRepository).updatePublishedStatus(eq(eventId), eq("Published"), any(LocalDateTime.class));
    }

    @Test
    void testExecuteAfterCommit_publishFails() {
        // Arrange
        String topic = "test-topic";
        String payload = "test-payload";
        String transactionId = "tx123";
        Long eventId = 123L;

        // Simulate serialization failure
        when(jsonHelper.convertToJson(any(Map.class))).thenThrow(new RuntimeException("Serialization error"));

        // Act
        executor.executeAfterCommit(topic, payload, transactionId, eventId, kafkaTemplate);

        // Trigger afterCommit manually
        for (TransactionSynchronization sync : TransactionSynchronizationManager.getSynchronizations()) {
            sync.afterCommit();
        }

        // Assert
        verify(kafkaTemplate, never()).send(anyString(), anyString(), any());

        // Assert: Internal event repository updated to "Publish_Failed"
        verify(internalEventRepository, timeout(3000)).updatePublishedStatus(eq(eventId), eq("Publish_Failed"), any(LocalDateTime.class));
    }
}

