package com.dpw.runner.shipment.services.kafka;

import com.dpw.runner.shipment.services.entity.InternalEvent;
import com.dpw.runner.shipment.services.executors.PostCommitActionExecutor;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaPayload;
import com.dpw.runner.shipment.services.kafka.producer.KafkaPublisher;
import com.dpw.runner.shipment.services.repository.interfaces.InternalEventRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.kafka.core.KafkaTemplate;

import java.time.LocalDateTime;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

class KafkaPublisherTest {

    @Mock
    private KafkaTemplate<String, Object> kafkaTemplate;

    @Mock
    private PostCommitActionExecutor postCommitExecutor;

    @Mock
    private InternalEventRepository internalEventRepository;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private TestKafkaPublisher publisher; // concrete subclass for testing

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    // A simple subclass of KafkaPublisher for testing
    static class TestKafkaPublisher extends KafkaPublisher<KafkaPayload> {
        @Override
        protected String getTopic() {
            return "test-topic";
        }
    }

    @Test
    void publish_ShouldPersistEventAndSchedulePostCommit() {
        // Arrange
        KafkaPayload payload = mock(KafkaPayload.class);
        String transactionId = "txn123";
        Long entityId = 101L;
        String entityType = "Shipment";

        when(jsonHelper.convertToJson(payload)).thenReturn("{\"field\":\"value\"}");

        InternalEvent savedEvent = InternalEvent.builder()
                .id(999L)
                .entityId(entityId)
                .entityType(entityType)
                .payload("{\"field\":\"value\"}")
                .publishedStatus("Publish_Pending")
                .publishedTimestamp(LocalDateTime.now())
                .consumedStatus("Consume_Pending")
                .consumedTimestamp(LocalDateTime.now())
                .build();
        when(internalEventRepository.save(any(InternalEvent.class))).thenReturn(savedEvent);

        // Act
        publisher.publish(payload, transactionId, entityId, entityType);

        // Assert
        verify(internalEventRepository).save(argThat(event ->
                event.getEntityId().equals(entityId) &&
                        event.getEntityType().equals(entityType) &&
                        event.getPublishedStatus().equals("Publish_Pending") &&
                        event.getConsumedStatus().equals("Consume_Pending")
        ));

        verify(postCommitExecutor).executeAfterCommit(
                "test-topic",
                payload,
                transactionId,
                999L,
                kafkaTemplate
        );

        verify(jsonHelper).convertToJson(payload);
    }

    @Test
    void publish_ShouldStillSchedulePostCommit_WhenEntityIdIsDifferent() {
        // Arrange
        KafkaPayload payload = mock(KafkaPayload.class);
        when(jsonHelper.convertToJson(payload)).thenReturn("{\"foo\":\"bar\"}");

        InternalEvent savedEvent = InternalEvent.builder()
                .id(500L)
                .entityId(200L)
                .entityType("Test")
                .payload("{\"foo\":\"bar\"}")
                .publishedStatus("Publish_Pending")
                .publishedTimestamp(LocalDateTime.now())
                .consumedStatus("Consume_Pending")
                .consumedTimestamp(LocalDateTime.now())
                .build();
        when(internalEventRepository.save(any(InternalEvent.class))).thenReturn(savedEvent);

        // Act
        publisher.publish(payload, "txn456", 200L, "Test");

        // Assert
        verify(postCommitExecutor).executeAfterCommit(
                "test-topic",
                payload,
                "txn456",
                500L,
                kafkaTemplate
        );
    }
}
