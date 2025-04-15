package com.dpw.runner.shipment.services.service_bus;

import com.azure.messaging.servicebus.*;
import com.azure.messaging.servicebus.administration.ServiceBusAdministrationClientBuilder;
import com.azure.messaging.servicebus.administration.models.SubscriptionRuntimeProperties;
import com.azure.messaging.servicebus.models.ServiceBusReceiveMode;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Data;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

@Data
@Component
@Generated
public class SBConfiguration {

    Map<String, ServiceBusSenderClient> senderMap = new HashMap<>();

    public ServiceBusSenderClient getSenderTopicClient(ISBProperties sbProperties, String topicName) {
        String key = sbProperties.getConnectionString()+"_"+topicName;
        ServiceBusSenderClient senderClient = senderMap.get(key);
        if(senderClient != null) {
            return senderClient;
        }
        ServiceBusSenderClient serviceBusSenderClient = new ServiceBusClientBuilder()
                .connectionString(sbProperties.getConnectionString())
                .sender()
                .topicName(topicName)
                .buildClient();
        senderMap.put(key, serviceBusSenderClient);
        return serviceBusSenderClient;
    }

    public ServiceBusProcessorClient getSessionProcessorClient(ISBProperties sbProperties, String topicName, String subName, Consumer<ServiceBusReceivedMessageContext> onMessage, Consumer<ServiceBusErrorContext> onError) {
        return new ServiceBusClientBuilder()
                .connectionString(sbProperties.getConnectionString())
                .sessionProcessor()
                .disableAutoComplete()
                .maxConcurrentSessions(64)
                .receiveMode(ServiceBusReceiveMode.PEEK_LOCK)
                .topicName(topicName)
                .subscriptionName(subName)
                .processMessage(onMessage)
                .processError(onError)
                .buildProcessorClient();
    }

    public ServiceBusProcessorClient getSessionProcessorClient(String connectionString, String topicName, String subName, Consumer<ServiceBusReceivedMessageContext> onMessage, Consumer<ServiceBusErrorContext> onError) {
        return new ServiceBusClientBuilder()
                .connectionString(connectionString)
                .processor()
                .disableAutoComplete()
                .maxAutoLockRenewDuration(Duration.ofMinutes(1))
                .maxConcurrentCalls(1)
                .receiveMode(ServiceBusReceiveMode.PEEK_LOCK)
                .topicName(topicName)
                .subscriptionName(subName)
                .processMessage(onMessage)
                .processError(onError)
                .buildProcessorClient();
    }

    public SubscriptionRuntimeProperties getSubscriptionRuntimeProperties(ISBProperties isbProperties, String topicName,
                                                                          String subscriptionName) {
        String connectionString = isbProperties.getConnectionString();
        return new ServiceBusAdministrationClientBuilder()
                .connectionString(connectionString)
                .buildClient().getSubscriptionRuntimeProperties(topicName,subscriptionName);
    }

    public ServiceBusProcessorClient getTopicReceiverClient(ISBProperties isbProperties,String topicName, String subscriptionName, Consumer<ServiceBusReceivedMessageContext> onMessage, Consumer<ServiceBusErrorContext> onError){

        String connectionString = isbProperties.getConnectionString();

        return new ServiceBusClientBuilder()
                .connectionString(connectionString)
                .processor()
                .topicName(topicName)
                .subscriptionName(subscriptionName)
                .disableAutoComplete()
                .receiveMode(ServiceBusReceiveMode.PEEK_LOCK)
                .processMessage(onMessage)
                .processError(onError)
                .buildProcessorClient();
    }
}
