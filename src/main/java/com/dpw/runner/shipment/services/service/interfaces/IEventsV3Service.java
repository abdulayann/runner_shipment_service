package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Transactional;

public interface IEventsV3Service {
    List<EventsResponse> listV2(CommonRequestModel commonRequestModel, String source);
    void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings);

    void processEventsAfterShipmentAttachment(Long consolidationId, ShipmentDetails shipmentDetails);

    @Transactional
    void saveAllEvent(List<EventsRequest> eventsRequests);

    @Transactional
    void saveEvent(EventsRequest eventsRequest);

    @Async
    CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> v1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;

    ResponseEntity<IRunnerResponse> trackEvents(TrackingEventsRequest request) throws RunnerException;

    ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException;
}
