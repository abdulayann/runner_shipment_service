package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Data
@Component
public class AzureServiceBusTopic {
    @Value("${data-sync-topic}")
    private String topic;

    @Value("${boomi-message-topic}")
    private String messageTopic;
}