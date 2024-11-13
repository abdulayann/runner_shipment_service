package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;

public interface IDpsEventService {

    DpsEvent saveDpsEvent(DpsDto dpsDto);
}
