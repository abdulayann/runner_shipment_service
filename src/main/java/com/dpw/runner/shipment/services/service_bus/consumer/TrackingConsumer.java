package com.dpw.runner.shipment.services.service_bus.consumer;

import com.azure.messaging.servicebus.ServiceBusErrorContext;
import com.azure.messaging.servicebus.ServiceBusProcessorClient;
import com.azure.messaging.servicebus.ServiceBusReceivedMessage;
import com.azure.messaging.servicebus.ServiceBusReceivedMessageContext;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service_bus.SBConfiguration;
import com.dpw.runner.shipment.services.service_bus.ServiceBusConfigProperties;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class TrackingConsumer {

    private final SBConfiguration sbConfiguration;
    private final JsonHelper jsonHelper;
    private final ServiceBusConfigProperties serviceBusConfigProperties;
    private final IEventService eventService;
    private ServiceBusProcessorClient processorClient;

    @Autowired
    TrackingConsumer(SBConfiguration sbConfiguration, JsonHelper jsonHelper,
            IEventService eventService, ServiceBusConfigProperties serviceBusConfigProperties) {
        this.sbConfiguration = sbConfiguration;
        this.jsonHelper = jsonHelper;
        this.eventService = eventService;
        this.serviceBusConfigProperties = serviceBusConfigProperties;
    }

    @PostConstruct
    public void startReceiver() {
        processorClient = sbConfiguration.getSessionProcessorClient(
                serviceBusConfigProperties.getTrackingService().getConnectionString(),
                serviceBusConfigProperties.getTrackingService().getTopicName(),
                serviceBusConfigProperties.getTrackingService().getSubscriptionName(),
                this::processMessage,
                this::processError
        );

        processorClient.start();

        log.info("Message receiver started and listening...");
    }

    public void processMessage(ServiceBusReceivedMessageContext context) {
        ServiceBusReceivedMessage receivedMessage = context.getMessage();
        log.info("Started processing message with id : {}", receivedMessage.getMessageId());

        TrackingServiceApiResponse.Container container = jsonHelper.readFromJson(receivedMessage.getBody().toString(), TrackingServiceApiResponse.Container.class);
        log.info("{}", jsonHelper.convertToJson(container));
        eventService.processUpstreamTrackingMessage(container);

        log.info("Finished processing message with id : {}", receivedMessage.getMessageId());
    }

    public void processError(ServiceBusErrorContext context) {
        // Process error
        log.error("Error occurred while processing message from tracking queue, with exception {}", context.getException().getMessage());
    }

    @PreDestroy
    public void stopConsumer() {
        processorClient.stop();
        processorClient.close();
    }

}
