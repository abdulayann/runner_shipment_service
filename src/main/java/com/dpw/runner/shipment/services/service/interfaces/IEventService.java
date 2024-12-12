package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import org.springframework.http.ResponseEntity;

public interface IEventService extends ICommonService {
    ResponseEntity<IRunnerResponse> V1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;
    ResponseEntity<IRunnerResponse> trackEvents(TrackingEventsRequest request) throws RunnerException;
    void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings);
    boolean processUpstreamTrackingMessage(Container container, String messageId);
    ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel);
}
