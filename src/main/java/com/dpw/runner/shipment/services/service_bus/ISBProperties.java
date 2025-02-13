package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;

@Data
public abstract class ISBProperties {
    @Value("${messaging-prop.shipment-config.runner.connectionString}")
    private String connectionString;
    private AzureServiceBusTopic topics;
}
