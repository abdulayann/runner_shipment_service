package com.dpw.runner.shipment.services.config;

import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.TopicBuilder;
import org.springframework.web.context.support.GenericWebApplicationContext;

import javax.annotation.PostConstruct;

@Configuration
public class TopicConfig {

    @Autowired
    private GenericWebApplicationContext context;

    private String MIN_IN_SYNC_REPLICAS_CONFIG = "MIN_IN_SYNC_REPLICAS_CONFIG";

    @Value("${awbKafka.queue}")
    private String awbKafkaBeanName;

    @Value("${shipmentsKafka.queue}")
    private String shipmentsKafkaBeanName;

    @Value("${consolidationsKafka.queue}")
    private String consolidationKafkaBeanName;

    @Value("${containersKafka.queue}")
    private String containerKafkaBeanName;

    @PostConstruct
    void init() {
        NewTopic awbTopic = TopicBuilder.name(awbKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(awbKafkaBeanName, NewTopic.class, () -> awbTopic);

        NewTopic shipmentsKafkaTopic = TopicBuilder.name(shipmentsKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(shipmentsKafkaBeanName, NewTopic.class, () -> shipmentsKafkaTopic);

        NewTopic consolidationKafkaTopic = TopicBuilder.name(consolidationKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(consolidationKafkaBeanName, NewTopic.class, () -> consolidationKafkaTopic);

        NewTopic containerKafkaTopic = TopicBuilder.name(containerKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(containerKafkaBeanName, NewTopic.class, () -> containerKafkaTopic);


    }


}