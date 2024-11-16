package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IDpsEventService {

    DpsEvent saveDpsEvent(DpsDto dpsDto);

    ResponseEntity<IRunnerResponse> getShipmentMatchingRulesByGuid(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateWarningRulesStatus(CommonRequestModel commonRequestModel);
}
