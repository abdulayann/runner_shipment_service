package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import org.apache.kafka.clients.admin.NewTopic;
import org.apache.kafka.common.config.TopicConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.TopicBuilder;
import org.springframework.web.context.support.GenericWebApplicationContext;

import javax.annotation.PostConstruct;

@Configuration
@Generated
public class TopicConfigs {

    @Autowired
    private GenericWebApplicationContext context;

    @Value("${awbKafka.queue}")
    private String awbKafkaBeanName;

    @Value("${shipmentsKafka.queue}")
    private String shipmentsKafkaBeanName;

    @Value("${consolidationsKafka.queue}")
    private String consolidationKafkaBeanName;

    @Value("${containersKafka.queue}")
    private String containerKafkaBeanName;

    @Value("${tiKafka.queue}")
    private String tiKafkaBeanName;

    @PostConstruct
    void init() {
        NewTopic awbTopic = TopicBuilder.name(awbKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(TopicConfig.MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(awbKafkaBeanName, NewTopic.class, () -> awbTopic);

        NewTopic shipmentsKafkaTopic = TopicBuilder.name(shipmentsKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(TopicConfig.MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(shipmentsKafkaBeanName, NewTopic.class, () -> shipmentsKafkaTopic);

        NewTopic consolidationKafkaTopic = TopicBuilder.name(consolidationKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(TopicConfig.MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(consolidationKafkaBeanName, NewTopic.class, () -> consolidationKafkaTopic);

        NewTopic containerKafkaTopic = TopicBuilder.name(containerKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(TopicConfig.MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(containerKafkaBeanName, NewTopic.class, () -> containerKafkaTopic);

        NewTopic tiKafkaTopic = TopicBuilder.name(tiKafkaBeanName)
                .partitions(3)
                .replicas(1)
                .config(TopicConfig.MIN_IN_SYNC_REPLICAS_CONFIG, String.valueOf(1))
                .compact()
                .build();
        context.registerBean(tiKafkaBeanName, NewTopic.class, () -> tiKafkaTopic);


    }


}