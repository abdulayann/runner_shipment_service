package com.dpw.runner.shipment.services.service_bus.consumer;

import com.azure.core.util.BinaryData;
import com.azure.messaging.servicebus.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service_bus.SBConfiguration;
import com.dpw.runner.shipment.services.service_bus.ServiceBusConfigProperties;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class TrackingConsumerTest {

    @InjectMocks
    TrackingConsumer trackingConsumer;

    @Mock
    private SBConfiguration sbConfiguration;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ServiceBusConfigProperties serviceBusConfigProperties;
    @Mock
    private IEventService eventService;

    private ServiceBusReceivedMessageContext contextMock;
    private ServiceBusReceivedMessage messageMock;
    private ServiceBusErrorContext errorMock;
    private ServiceBusProcessorClient processorMock;


    @BeforeEach
    public void setup() {
        contextMock = Mockito.mock(ServiceBusReceivedMessageContext.class);
        messageMock = Mockito.mock(ServiceBusReceivedMessage.class);
        errorMock = Mockito.mock(ServiceBusErrorContext.class);
    }

    @Test
    void startReceiver() {
        ServiceBusConfigProperties.TrackingService config = new ServiceBusConfigProperties.TrackingService();
        config.setConnectionString("");
        config.setTopicName("");
        config.setSubscriptionName("");

        processorMock = Mockito.mock(ServiceBusProcessorClient.class);

        when(serviceBusConfigProperties.getTrackingService()).thenReturn(config);
        when(sbConfiguration.getSessionProcessorClient(anyString(), anyString(), anyString(), any(), any())).thenReturn(processorMock);

        trackingConsumer.startReceiver();

        verify(processorMock).start();
    }

    @Test
    void testProcessMessageMarksMessageComplete() {
        // Happy flow
        when(contextMock.getMessage()).thenReturn(messageMock); // Mock the message retrieval
        when(messageMock.getBody()).thenReturn(BinaryData.fromString("This is a valid message"));
        when(eventService.processUpstreamTrackingMessage(any())).thenReturn(true);

        trackingConsumer.processMessage(contextMock);

        Mockito.verify(contextMock).complete(); // Ensure complete() is called
    }

    @Test
    void testProcessMessageMarksMessageCompleteIfItsNotUsefulToBeRetriedLater() {
        // Given an invalid message
        when(contextMock.getMessage()).thenReturn(messageMock); // Mock the message retrieval
        when(messageMock.getBody()).thenReturn(BinaryData.fromString("This is an invalid message"));
        when(eventService.processUpstreamTrackingMessage(any())).thenReturn(true);

        trackingConsumer.processMessage(contextMock);

        Mockito.verify(contextMock).complete(); // Ensure complete() is called
    }

    @Test
    void testProcessMessageAllowsMessageToBeRetriedInFuture() {
        // Given an invalid message
        when(contextMock.getMessage()).thenReturn(messageMock); // Mock the message retrieval
        when(messageMock.getBody()).thenReturn(BinaryData.fromString("This is a valid message"));
        when(eventService.processUpstreamTrackingMessage(any())).thenReturn(false);

        trackingConsumer.processMessage(contextMock);

        Mockito.verify(contextMock, never()).complete();
    }

    @Test
    void testProcessError_ServiceBusException() {
        // Mock ServiceBusException
        ServiceBusException serviceBusException = Mockito.mock(ServiceBusException.class);
        when(errorMock.getException()).thenReturn(serviceBusException);
        when(errorMock.getErrorSource()).thenReturn(ServiceBusErrorSource.RECEIVE);
        when(serviceBusException.getReason()).thenReturn(ServiceBusFailureReason.MESSAGE_LOCK_LOST);

        // Call method
        trackingConsumer.processError(errorMock);

        // Verify logs
        verify(errorMock).getException();
        verify(errorMock).getErrorSource();
        verify(serviceBusException).getReason();
    }

    @Test
    void testProcessError_OtherException() {
        // Mock a generic exception
        Exception genericException = new RuntimeException("Test exception");
        when(errorMock.getException()).thenReturn(genericException);

        // Call method
        trackingConsumer.processError(errorMock);

        // Verify that the getMessage method on the exception is called
        verify(errorMock, atLeastOnce()).getException();
        verify(errorMock, never()).getErrorSource();
    }


    @Test
    void stopConsumer() {
        processorMock = Mockito.mock(ServiceBusProcessorClient.class);
        ReflectionTestUtils.setField(trackingConsumer, "processorClient", processorMock);

        trackingConsumer.stopConsumer();

        verify(processorMock).stop();
        verify(processorMock).close();
    }
}