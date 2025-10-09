package com.dpw.runner.shipment.services.kafka.config;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.HashMap;
import java.util.Map;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.common.serialization.StringDeserializer;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.listener.ContainerProperties.AckMode;

@Configuration
@Generated
public class ConsumerConfiguration {

    @Value("${shipmentsKafka.bootstrapServer}")
    private String bootstrapServerConfig;
    @Value("${data.sync.kafka.subs}")
    private String dataSyncConsumer;
    @Value("${kafka.max.poll.interval.ms}")
    private Integer maxPollInterval;
    @Value("${custom.service.container.event.kafka.subs}")
    private String customServiceContainerEventGroupId;
    @Value("${dps.kafka.group-id}")
    private String dpsKafkaGroupId;
    @Value("${document.master.kafka.subs}")
    private String documentKafkaGroupId;
    @Value("${bill.common-event.kafka.group-id}")
    private String billCommonEventKafkaGroupId;
    @Value("${bridge.inttra.messages.kafka.consumer-group}")
    private String inttraConsumerGroupId;

    @Bean
    public ConsumerFactory<String, String> consumerFactory() {
        Map<String, Object> props = new HashMap<>();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        props.put(ConsumerConfig.GROUP_ID_CONFIG, dataSyncConsumer);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, maxPollInterval);
        return new DefaultKafkaConsumerFactory<>(props);
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> kafkaListenerContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<String, String> factory =
                new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory());
        return factory;
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> customServiceContainerEventKafkaListenerContainerFactory() {

        return getConcurrentKafkaListenerContainerFactory(customServiceContainerEventConsumerFactory());
    }

    @Bean
    public ConsumerFactory<String, String> customServiceContainerEventConsumerFactory() {
        Map<String, Object> props = new HashMap<>();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.GROUP_ID_CONFIG, customServiceContainerEventGroupId);

        // Reset the offset to the earliest available offset on the partition if no offset is stored
        // This is useful when a new consumer group is created, and you want to consume all messages from the beginning
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, Constants.AUTO_OFFSET_RESET_CONFIG_LATEST);
        return new DefaultKafkaConsumerFactory<>(props, new StringDeserializer(), new StringDeserializer());
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> documentKafkaListenerContainerFactory() {
        return getConcurrentKafkaListenerContainerFactory(documentConsumerFactory());
    }
    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> bridgeServiceContainerFactory() {
        return getConcurrentKafkaListenerContainerFactory(inttraConsumerFactory());
    }

    @NotNull
    private ConcurrentKafkaListenerContainerFactory<String, String> getConcurrentKafkaListenerContainerFactory(ConsumerFactory<String, String> consumerFactory) {
        ConcurrentKafkaListenerContainerFactory<String, String> factory = new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory);
        factory.setConcurrency(1);
        factory.getContainerProperties().setAckMode(AckMode.MANUAL_IMMEDIATE);
        return factory;
    }

    @Bean
    public ConsumerFactory<String, String> documentConsumerFactory() {
        Map<String, Object> props = new HashMap<>();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.GROUP_ID_CONFIG, documentKafkaGroupId);
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, Constants.AUTO_OFFSET_RESET_CONFIG_LATEST);
        return new DefaultKafkaConsumerFactory<>(props, new StringDeserializer(), new StringDeserializer());
    }
    @Bean
    public ConsumerFactory<String, String> inttraConsumerFactory() {
        Map<String, Object> props = new HashMap<>();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.GROUP_ID_CONFIG, inttraConsumerGroupId);
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, Constants.AUTO_OFFSET_RESET_CONFIG_LATEST);
        return new DefaultKafkaConsumerFactory<>(props, new StringDeserializer(), new StringDeserializer());
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> dpsKafkaListenerContainerFactory() {

        return getConcurrentKafkaListenerContainerFactory(dpsConsumerFactory());
    }

    @Bean
    public ConsumerFactory<String, String> dpsConsumerFactory() {
        Map<String, Object> props = new HashMap<>();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.GROUP_ID_CONFIG, dpsKafkaGroupId);

        // Reset the offset to the earliest available offset on the partition if no offset is stored
        // This is useful when a new consumer group is created, and you want to consume all messages from the beginning
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, Constants.AUTO_OFFSET_RESET_CONFIG_LATEST);
        return new DefaultKafkaConsumerFactory<>(props, new StringDeserializer(), new StringDeserializer());
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> billingCommonEventKafkaListenerContainerFactory() {
        return getConcurrentKafkaListenerContainerFactory(billingCommonEventConsumerFactory());
    }

    @Bean
    public ConsumerFactory<String, String> billingCommonEventConsumerFactory() {
        Map<String, Object> props = new HashMap<>();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.GROUP_ID_CONFIG, billCommonEventKafkaGroupId);
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        return new DefaultKafkaConsumerFactory<>(props, new StringDeserializer(), new StringDeserializer());
    }


}
