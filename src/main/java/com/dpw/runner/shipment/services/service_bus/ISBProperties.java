package com.dpw.runner.shipment.services.service_bus;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import org.springframework.beans.factory.annotation.Value;

import java.util.Map;

@Data
public abstract class ISBProperties {
    @Value("${messaging-prop.shipment-config.runner.connectionString}")
    private String connectionString;
    private AzureServiceBusTopic topics;
}
