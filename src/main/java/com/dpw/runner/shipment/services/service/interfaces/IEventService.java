package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IEventService extends ICommonService {
    ResponseEntity<IRunnerResponse> V1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;
    ResponseEntity<IRunnerResponse> trackEvents(TrackingEventsRequest request) throws RunnerException;

    String convertTrackingEventCodeToShortCode(String locationRole, String eventCode);

    void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings);
}
