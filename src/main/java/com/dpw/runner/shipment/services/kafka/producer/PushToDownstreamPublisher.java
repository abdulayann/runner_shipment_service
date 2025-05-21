package com.dpw.runner.shipment.services.kafka.producer;

import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class PushToDownstreamPublisher extends KafkaPublisher<PushToDownstreamEventDto> {

    @Value("${shipments.internal.messages.kafka}")
    private String topic;

    @Override
    protected String getTopic() {
        return topic;
    }
}
