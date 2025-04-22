package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.GetMatchingRulesRequest;
import com.dpw.runner.shipment.services.dto.response.DpsEventResponse;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import java.util.List;
import java.util.Set;
import org.springframework.http.ResponseEntity;

public interface IDpsEventService {

    DpsEvent saveDpsEvent(DpsDto dpsDto);

    List<String> getImplicationsForShipment(String entityId);

    Boolean isImplicationPresent(List<Long> shipmentIds, String implication);

    Boolean isImplicationPresent(Set<String> shipmentGuids, String implication);

    void createAuditLog(DpsEvent dpsEvent, ShipmentDetails shipmentDetails);

    ResponseEntity<IRunnerResponse> getShipmentMatchingRulesByGuid(String shipmentGuid);

    ResponseEntity<IRunnerResponse> getShipmentMatchingRulesByGuidAndExecutionState(GetMatchingRulesRequest getMatchingRulesRequest);

    List<DpsEvent> findDpsEventByGuidAndExecutionState(String shipmentGuid, DpsExecutionStatus dpsExecutionStatus);

    DpsEventResponse constructDpsEventResponse(DpsEvent dpsEvent);

    DpsEvent constructDpsEvent(DpsDto dpsDto);
}
