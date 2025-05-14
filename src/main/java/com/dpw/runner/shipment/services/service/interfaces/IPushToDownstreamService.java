package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;

public interface IPushToDownstreamService {
    public void process(PushToDownstreamEventDto message);
}
