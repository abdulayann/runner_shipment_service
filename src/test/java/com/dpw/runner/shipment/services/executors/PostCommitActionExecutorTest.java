package com.dpw.runner.shipment.services.executors;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaEventPublisherService;
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

import java.util.List;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PostCommitActionExecutorTest {

    @Mock
    private IKafkaEventPublisherService kafkaEventPublisherService;

    @InjectMocks
    private PostCommitActionExecutor executor;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        // Ensure transaction synchronization is active for testing
        if (!TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.initSynchronization();
        }
    }

    @Test
    void testExecuteAfterCommit_successfulPublish() throws Exception {
        String topic = "test-topic";
        String payload = "test-payload";
        String transactionId = "tx123";
        Long eventId = 123L;

        // Act
        executor.executeAfterCommit(topic, payload, transactionId, eventId);

        // Trigger afterCommit manually
        List<TransactionSynchronization> syncs = TransactionSynchronizationManager.getSynchronizations();
        for (TransactionSynchronization sync : syncs) {
            sync.afterCommit();
        }

        // Verify async publish is called
        verify(kafkaEventPublisherService).publishToKafka(topic, payload, transactionId, eventId);
    }

    @Test
    void testExecuteAfterCommit_publishFails() throws Exception {
        String topic = "test-topic";
        String payload = "test-payload";
        String transactionId = "tx123";
        Long eventId = 123L;

        // Simulate an exception thrown during publish
        doThrow(new RuntimeException("Kafka publish failed"))
                .when(kafkaEventPublisherService)
                .publishToKafka(topic, payload, transactionId, eventId);

        // Act
        executor.executeAfterCommit(topic, payload, transactionId, eventId);

        // Trigger afterCommit manually
        List<TransactionSynchronization> syncs = TransactionSynchronizationManager.getSynchronizations();
        for (TransactionSynchronization sync : syncs) {
            sync.afterCommit();
        }

        // Verify publish was attempted despite exception
        verify(kafkaEventPublisherService, timeout(3000)).publishToKafka(topic, payload, transactionId, eventId);
    }

    @Test
    void testExecuteAfterCommit_noActiveTransaction_executesImmediately() {
        // Clear transaction synchronizations to simulate no active transaction
        TransactionSynchronizationManager.clearSynchronization();

        String topic = "test-topic";
        String payload = "payload";
        String transactionId = "tx123";
        Long eventId = 456L;

        executor.executeAfterCommit(topic, payload, transactionId, eventId);

        // Verify publish called immediately
        verify(kafkaEventPublisherService).publishToKafka(topic, payload, transactionId, eventId);
    }
}

