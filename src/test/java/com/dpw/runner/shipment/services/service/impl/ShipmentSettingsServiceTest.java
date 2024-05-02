package com.dpw.runner.shipment.services.service.impl;

import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class ShipmentSettingsServiceTest {

    @InjectMocks
    private ShipmentSettingsService shipmentSettingsService;
}
