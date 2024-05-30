package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;

import java.util.ArrayList;
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

}
