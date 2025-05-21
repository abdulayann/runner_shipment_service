package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;

public interface IPushToDownstreamService {

    void process(PushToDownstreamEventDto message, String transactionId);

    void pushContainerData(PushToDownstreamEventDto eventDto, String transactionId);

    void pushConsolidationData(PushToDownstreamEventDto message, String transactionId);

    void pushConsolidationDataToTracking(PushToDownstreamEventDto eventDto, String transactionId);
}
