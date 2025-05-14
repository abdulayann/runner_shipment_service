package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.service.interfaces.IPushToDownstreamService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class PushToDownstreamService implements IPushToDownstreamService {

    @Override
    public void process(PushToDownstreamEventDto message) {
        // Process the messages based on parent entity

    }
}
