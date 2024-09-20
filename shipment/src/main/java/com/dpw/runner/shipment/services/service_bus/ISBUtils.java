package com.dpw.runner.shipment.services.service_bus;

import com.azure.messaging.servicebus.ServiceBusErrorContext;
import com.azure.messaging.servicebus.ServiceBusMessage;
import com.azure.messaging.servicebus.ServiceBusReceivedMessageContext;

import java.util.List;
import java.util.function.Consumer;

public interface ISBUtils {

    void startSessionReceiving(
            ISBProperties isbProperties,
            String topicName,
            String subscriptionName,
            Consumer<ServiceBusReceivedMessageContext> onMessage,
            Consumer<ServiceBusErrorContext> onError
    ) throws InterruptedException;

    void sendMessagesToTopic(ISBProperties isbProperties, String topicName, List<ServiceBusMessage> messages);

    void onServiceBusErrorContext(ServiceBusErrorContext context);

}
