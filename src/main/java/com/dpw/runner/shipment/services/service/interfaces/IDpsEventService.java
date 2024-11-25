package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.DpsEventResponse;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import java.util.List;
import org.springframework.http.ResponseEntity;

public interface IDpsEventService {

    DpsEvent saveDpsEvent(DpsDto dpsDto);

    List<String> getImplicationsForShipment(String entityId);

    void createAuditLog(DpsEvent dpsEvent, ShipmentDetails shipmentDetails);

    ResponseEntity<IRunnerResponse> getShipmentMatchingRulesByGuid(String shipmentGuid);

    DpsEventResponse constructDpsEventResponse(DpsEvent dpsEvent);

    DpsEvent constructDpsEvent(DpsDto dpsDto);
}
