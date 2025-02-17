package com.dpw.runner.shipment.services.kafka.config;

import com.dpw.runner.shipment.services.kafka.serializer.ProducerSerializer;
import com.dpw.runner.shipment.services.utils.Generated;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;

import java.util.HashMap;
import java.util.Map;

@Configuration
@Generated
public class ProducerConfiguration {

    @Value("${shipmentsKafka.bootstrapServer}")
    private String bootstrapServerConfig;

    @Bean
    public <T> ProducerFactory<String, Object> producerFactory() {

        Map<String, Object> configProps = new HashMap<>();
        configProps.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServerConfig);
        configProps.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        configProps.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, ProducerSerializer.class.getName());
        configProps.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        return new DefaultKafkaProducerFactory<String, Object>(configProps);
    }

    @Bean
    public <T> KafkaTemplate<String, Object> kafkaTemplate() {

        return new KafkaTemplate<String, Object>(producerFactory());

    }

}
