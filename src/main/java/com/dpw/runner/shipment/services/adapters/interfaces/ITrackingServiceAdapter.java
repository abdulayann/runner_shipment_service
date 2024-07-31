package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.dto.TrackingService.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.commons.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.commons.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.commons.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.commons.entity.Events;
import com.dpw.runner.shipment.services.commons.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;

public interface ITrackingServiceAdapter {

    void publishUpdatesToTrackingServiceQueue(String jsonBody,Boolean isTrackingEvents);
    boolean checkIfConsolAttached(ShipmentDetails shipmentDetails);
    boolean checkIfAwbExists(ConsolidationDetails consolidationDetails);
    boolean checkIfConsolContainersExist(ConsolidationDetails consolidationDetails);
    UniversalTrackingPayload mapConsoleDataToTrackingServiceData(ConsolidationDetails consolidationDetails);
    UniversalTrackingPayload mapShipmentDataToTrackingServiceData(ShipmentDetails shipmentDetails);
    List<Events> getAllEvents(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String refNumber);
    UniversalTrackingPayload.UniversalEventsPayload mapEventDetailsForTracking(String bookingReferenceNumber, String referenceNumberType, String runnerReferenceNumber, List<Events> events);

    TrackingServiceApiResponse fetchTrackingData(TrackingRequest request) throws RunnerException;
}
