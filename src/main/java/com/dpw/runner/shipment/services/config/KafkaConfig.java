package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import org.apache.kafka.clients.admin.AdminClientConfig;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.record.CompressionType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.KafkaAdmin;

import java.util.HashMap;
import java.util.Map;

@Configuration
@Generated
public class KafkaConfig {
    @Value(value = "${shipmentsKafka.bootstrapServer}")
    private String bootstrapAddress;

    @Bean
    public KafkaAdmin kafkaAdmin() {
        Map<String, Object> configs = new HashMap<>();
        configs.put(AdminClientConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapAddress);
        configs.put(ProducerConfig.COMPRESSION_TYPE_CONFIG, CompressionType.GZIP.name);

        return new KafkaAdmin(configs);
    }
}