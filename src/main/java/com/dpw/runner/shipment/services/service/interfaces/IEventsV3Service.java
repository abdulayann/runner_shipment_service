package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;

public interface IEventsV3Service {
    List<EventsResponse> listV2(CommonRequestModel commonRequestModel, String source);
    void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings);

    void processEventsAfterShipmentAttachment(Long consolidationId, ShipmentDetails shipmentDetails);

    @Transactional
    void saveAllEvent(List<EventsRequest> eventsRequests);

    @Transactional
    void saveEvent(EventsRequest eventsRequest);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException;
}
