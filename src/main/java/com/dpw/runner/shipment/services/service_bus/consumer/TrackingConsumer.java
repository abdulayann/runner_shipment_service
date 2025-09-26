package com.dpw.runner.shipment.services.service_bus.consumer;

import com.azure.messaging.servicebus.ServiceBusErrorContext;
import com.azure.messaging.servicebus.ServiceBusException;
import com.azure.messaging.servicebus.ServiceBusProcessorClient;
import com.azure.messaging.servicebus.ServiceBusReceivedMessage;
import com.azure.messaging.servicebus.ServiceBusReceivedMessageContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.IgnoreAutoTenantPopulationContext;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.SBConfiguration;
import com.dpw.runner.shipment.services.service_bus.ServiceBusConfigProperties;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.File;
import java.io.IOException;
import java.util.UUID;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
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

    @Value("${tracking.event.abs.consumer-auto-startup}")
    private Boolean startConsumer;

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
        if (Boolean.TRUE.equals(startConsumer)) {
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
        String messageId = UUID.randomUUID().toString();
        MDC.put(LoggingConstants.TS_ID, messageId);

        log.info("Tracking Consumer - Started processing Bus id: {} message with id : {} Raw data {}", receivedMessage.getMessageId(), messageId,
                receivedMessage.getBody());
        TrackingServiceApiResponse.Container container = jsonHelper.readFromJson(receivedMessage.getBody().toString(), TrackingServiceApiResponse.Container.class);
//        Container container = fetchTrackingData();
        log.info("Tracking Consumer - container payload {} messageId {}", jsonHelper.convertToJson(container), messageId);
        v1Service.setAuthContext();
        // IMPORTANT: This context disables the auto population of tenant Id while saving.
        IgnoreAutoTenantPopulationContext.setContext(Boolean.TRUE);
        boolean processSuccess = eventService.processUpstreamTrackingMessage(container, messageId);

        if(processSuccess) {
            context.complete();
            log.info("Tracking Consumer - Finished processing message with id : {}", messageId);
        }
        v1Service.clearAuthContext();
        IgnoreAutoTenantPopulationContext.clearContext();
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

    //    @PostConstruct
    public TrackingServiceApiResponse.Container fetchTrackingData() throws RunnerException {
        Container container = fetchFromJsonFile("src/main/resources/ts_payload_sample.json");

        eventService.processUpstreamTrackingMessage(container, "messageId");

        return container;
    }

    private TrackingServiceApiResponse.Container fetchFromJsonFile(String filePath) throws RunnerException {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            objectMapper.registerModule(new JavaTimeModule());
            // Assuming the JSON file is on the classpath, adjust if needed

            return objectMapper.readValue(new File(filePath), TrackingServiceApiResponse.Container.class);
        } catch (IOException e) {
            log.error("Error reading JSON file", e);
            throw new RunnerException("Error reading JSON file: " + e.getMessage());
        }
    }

}
