package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Data
@Component
public class AzureServiceBusTopic {
    @Value("cargoes-abbs-vessel-test-topic")
    private String topic;
    @Value("abbs-backend-subs")
    private String subscription;
}