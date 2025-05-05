package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import java.util.List;
import org.springframework.transaction.annotation.Transactional;

public interface IEventsV3Service {
    List<EventsResponse> listV2(CommonRequestModel commonRequestModel, String source);

    void processEventsAfterShipmentAttachment(Long consolidationId, ShipmentDetails shipmentDetails);

    @Transactional
    void saveAllEvent(List<EventsRequest> eventsRequests);
}
