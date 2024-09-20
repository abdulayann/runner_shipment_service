package com.dpw.runner.shipment.services.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SyncConfig {

    @Value("${reverse.sync.active}")
    public Boolean IS_REVERSE_SYNC_ACTIVE;

}