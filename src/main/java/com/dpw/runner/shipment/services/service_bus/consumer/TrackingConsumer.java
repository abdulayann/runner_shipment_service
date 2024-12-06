package com.dpw.runner.shipment.services.service_bus.consumer;

import com.azure.messaging.servicebus.*;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.SBConfiguration;
import com.dpw.runner.shipment.services.service_bus.ServiceBusConfigProperties;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class TrackingConsumer {

    private final SBConfiguration sbConfiguration;
    private final JsonHelper jsonHelper;
    private final ServiceBusConfigProperties serviceBusConfigProperties;
    private final IEventService eventService;
    private ServiceBusProcessorClient processorClient;
    private final IV1Service v1Service;
    @Value("${thread.sleep.time.ms}")
    private long TIMEOUT_MS;

    @Autowired
    TrackingConsumer(SBConfiguration sbConfiguration, JsonHelper jsonHelper,
            IEventService eventService, ServiceBusConfigProperties serviceBusConfigProperties, IV1Service v1Service) {
        this.sbConfiguration = sbConfiguration;
        this.jsonHelper = jsonHelper;
        this.eventService = eventService;
        this.serviceBusConfigProperties = serviceBusConfigProperties;
        this.v1Service = v1Service;
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

        log.info("Tracking Consumer - started and listening...");
    }

    /**
     * @param context
     * If we get a true response when processing the push message we mark the message as complete by calling complete()
     * Otherwise we don't explicitly modify any message metadata, and this should be available to this consumer to be
     * reprocessed next time unless the delivery count exceeds 10 and forces it to be dead lettered.
     */
    @SneakyThrows
    public void processMessage(ServiceBusReceivedMessageContext context) {
        ServiceBusReceivedMessage receivedMessage = context.getMessage();
        log.info("Tracking Consumer - Started processing message with id : {}", receivedMessage.getMessageId());

        TrackingServiceApiResponse.Container container = jsonHelper.readFromJson(receivedMessage.getBody().toString(), TrackingServiceApiResponse.Container.class);
        log.info("Tracking Consumer - container payload {}", jsonHelper.convertToJson(container));
        v1Service.setAuthContext();
        boolean processSuccess = eventService.processUpstreamTrackingMessage(container);

        if(processSuccess) {
            context.complete();
            log.info("Tracking Consumer - Finished processing message with id : {}", receivedMessage.getMessageId());
        }
        v1Service.clearAuthContext();
    }

    public void processError(ServiceBusErrorContext context) {
        // Process error
        if (context.getException() instanceof ServiceBusException exception) {
            log.error("Tracking Consumer - Error source: {}, reason {}", context.getErrorSource(), exception.getReason(), exception.getCause());
        }
        else {
            log.error("Tracking Consumer - Error occurred while processing message from tracking queue, with exception {}", context.getException().getMessage(), context.getException().getCause());
        }
        v1Service.clearAuthContext();
    }

    @PreDestroy
    public void stopConsumer() {
        processorClient.stop();
        processorClient.close();
    }

}
