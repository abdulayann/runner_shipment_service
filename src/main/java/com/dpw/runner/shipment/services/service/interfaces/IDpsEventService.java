package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;

import java.util.List;

public interface IDpsEventService {

    DpsEvent saveDpsEvent(DpsDto dpsDto);

    List<DpsEvent> getMatchingRulesByGuid(CommonRequestModel commonRequestModel);
}
