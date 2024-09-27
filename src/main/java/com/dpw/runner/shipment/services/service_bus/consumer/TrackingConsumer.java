package com.dpw.runner.shipment.services.service_bus.consumer;

import com.azure.messaging.servicebus.*;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service_bus.SBConfiguration;
import com.dpw.runner.shipment.services.service_bus.TrackingServiceProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

@Service
@Slf4j
public class TrackingConsumer {

    private final SBConfiguration sbConfiguration;
    private final JsonHelper jsonHelper;
    private final TrackingServiceProperties trackingServiceProperties;
    private final IEventService eventService;

    private ServiceBusProcessorClient processorClient;


    @Autowired
    TrackingConsumer(SBConfiguration sbConfiguration, JsonHelper jsonHelper, TrackingServiceProperties trackingServiceProperties,
                     IEventService eventService) {
        this.sbConfiguration = sbConfiguration;
        this.jsonHelper = jsonHelper;
        this.trackingServiceProperties = trackingServiceProperties;
        this.eventService = eventService;
    }

    @PostConstruct
    public void startReceiver() {
        processorClient = sbConfiguration.getSessionProcessorClient(
                trackingServiceProperties.getConnectionString(),
                trackingServiceProperties.getTopicName(),
                trackingServiceProperties.getSubName(),
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
