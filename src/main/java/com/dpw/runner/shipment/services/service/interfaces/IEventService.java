package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto;
import java.util.List;
import javax.validation.Valid;
import org.springframework.http.ResponseEntity;

public interface IEventService extends ICommonService {
    ResponseEntity<IRunnerResponse> v1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;
    ResponseEntity<IRunnerResponse> trackEvents(TrackingEventsRequest request) throws RunnerException;
    void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings);
    boolean processUpstreamTrackingMessage(Container container, String messageId);
    ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel);
    void processUpstreamBillingCommonEventMessage(BillingInvoiceDto billingInvoiceDto);
    void saveEvent(EventsRequest eventsRequest);

    void populateBranchNames(List<EventsResponse> eventResponses);
    ResponseEntity<IRunnerResponse> pushTrackingEvents(@Valid Container request);
    void saveAllEvent(List<EventsRequest> eventsRequests);
}
