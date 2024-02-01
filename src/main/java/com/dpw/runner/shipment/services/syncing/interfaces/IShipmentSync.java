package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.UUID;

public interface IShipmentSync {
    ResponseEntity<?> sync(ShipmentDetails shipmentDetails, List<UUID> deletedContGuids, List<NotesRequest> customerBookingNotes, String transactionId);
    void syncLockStatus(ShipmentDetails shipmentDetails);
}
