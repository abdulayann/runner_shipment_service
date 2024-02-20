package com.dpw.runner.shipment.services.service_bus;

import com.azure.messaging.servicebus.*;
import com.azure.messaging.servicebus.administration.models.QueueRuntimeProperties;
import com.azure.messaging.servicebus.administration.models.SubscriptionRuntimeProperties;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@Component
@Slf4j
@SuppressWarnings("InfiniteLoopStatement")
@EnableAsync
public class SBUtilsImpl implements ISBUtils {

    @Autowired
    SBConfiguration sbConfiguration;

    @Async
    public void sendMessagesToTopic(ISBProperties sbProperties, String topicName, List<ServiceBusMessage> messages) {
        // create a Service Bus Sender client for the queue
        ServiceBusSenderClient senderClient = sbConfiguration.getSenderTopicClient(sbProperties, topicName);

        // Creates an ServiceBusMessageBatch where the ServiceBus.
        ServiceBusMessageBatch messageBatch = senderClient.createMessageBatch();

        // We try to add as many messages as a batch can fit based on the maximum size and send to Service Bus when
        // the batch can hold no more messages. Create a new batch for next set of messages and repeat until all
        // messages are sent.
        for (ServiceBusMessage message : messages) {
            if (messageBatch.tryAddMessage(message)) {
                continue;
            }

            // The batch is full, so we create a new batch and send the batch.
            senderClient.sendMessages(messageBatch);
            System.out.println("Sent a batch of messages to the topic: " + topicName);

            // create a new batch
            messageBatch = senderClient.createMessageBatch();

            // Add that message that we couldn't before.
            if (!messageBatch.tryAddMessage(message)) {
                log.error("ERROR | Message is too large for an empty batch. Skipping. Max size: {}.", messageBatch.getMaxSizeInBytes());
                System.err.printf("Message is too large for an empty batch. Skipping. Max size: %s.", messageBatch.getMaxSizeInBytes());
            }
        }
        if (messageBatch.getCount() > 0) {
            var messagesList = messages.stream().map(c -> StringUtility.convertToString(c.getBody())).toList();
            senderClient.sendMessages(messageBatch);
            log.info("Sent a batch of messages to the topic: {} with messages: {}" , topicName, String.join(", ", messagesList));
            System.out.println("Sent a batch of messages to the topic: " + topicName);
        }
        //close the client
        senderClient.close();
    }

    @Override
    public void startSessionReceiving(
            ISBProperties sbProperties,
            String topicName,
            String subscriptionName,
            Consumer<ServiceBusReceivedMessageContext> onMessage,
            Consumer<ServiceBusErrorContext> onError) throws InterruptedException {

        ExecutorService executorService = Executors.newSingleThreadExecutor();
        executorService.submit(() -> {
            log.info("Executor thread calling to start listener {}", topicName);
            while (true) {
                try {
                    ServiceBusProcessorClient sessionProcessor = null;
                    log.info("1.starting processor client for {} ", topicName);
                    sessionProcessor = sbConfiguration.getSessionProcessorClient(sbProperties, topicName, subscriptionName, onMessage, onError);
                    sessionProcessor.start();
                    TimeUnit.MINUTES.sleep(30);
                    log.info("2.shutting down processor client for {} ", topicName);
                    sessionProcessor.close();
                } catch (Exception ex) {
                    log.error("Exception occurred in sleep thread for service bus", ex);
                }
            }
        });
    }

    public void onServiceBusErrorContext(ServiceBusErrorContext context) {
        log.info("Error when receiving messages from namespace: '{}'. Entity: '{}'",
                 context.getFullyQualifiedNamespace(), context.getEntityPath());

        if (context.getException() instanceof ServiceBusException) {
            ServiceBusException exception = (ServiceBusException) context.getException();
            log.info("Error source: {}, reason {}", context.getErrorSource(),
                     exception.getReason());
        } else {
            log.info("Error occurred: %s%n", context.getException());
        }
    }
}
