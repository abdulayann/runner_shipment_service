package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.response.TrackingEventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;

public interface ITrackingServiceAdapter {

    void publishUpdatesToTrackingServiceQueue(String jsonBody, Boolean isTrackingEvents);

    boolean checkIfConsolAttached(ShipmentDetails shipmentDetails);

    boolean checkIfAwbExists(ConsolidationDetails consolidationDetails);

    boolean checkIfConsolContainersExist(ConsolidationDetails consolidationDetails);

    UniversalTrackingPayload mapConsoleDataToTrackingServiceData(ConsolidationDetails consolidationDetails, ShipmentDetails shipmentDetails);

    UniversalTrackingPayload mapShipmentDataToTrackingServiceData(ShipmentDetails shipmentDetails);

    List<Events> getAllEvents(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String refNumber);

    UniversalTrackingPayload.UniversalEventsPayload mapEventDetailsForTracking(String bookingReferenceNumber, String referenceNumberType, String runnerReferenceNumber, List<Events> events);

    TrackingServiceApiResponse fetchTrackingData(TrackingRequest request) throws RunnerException;

    String convertTrackingEventCodeToShortCode(String locationRole, String eventCode, String description);

    TrackingEventsResponse getTrackingEventsResponse(String referenceNumber) throws RunnerException;

    List<Events> generateEventsFromTrackingResponse(TrackingServiceApiResponse trackingServiceApiResponse);
}
