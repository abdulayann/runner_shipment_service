package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Awb;

import java.util.List;

public interface IKafkaAsyncService {
    void pushToKafkaAwb(Awb awb, boolean isCreate);
    void pushToKafkaTI(List<IRunnerResponse> pickupDeliveryDetails, boolean isCreate, Long shipmentId);
}