package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Data
@Component
public class AzureServiceBusTopic {
    @Value("${data-sync-topic}")
    private String topic;

}