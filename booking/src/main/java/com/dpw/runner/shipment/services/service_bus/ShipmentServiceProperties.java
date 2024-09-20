package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@EqualsAndHashCode(callSuper = true)
@Component()
@ConfigurationProperties(prefix = "messaging-prop.asb-config.abbs")
@Data
public class ShipmentServiceProperties extends ISBProperties {
}
