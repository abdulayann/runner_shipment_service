package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.UUID;

public interface IShipmentSync {
    ResponseEntity<IRunnerResponse> sync(ShipmentDetails shipmentDetails, List<UUID> deletedContGuids, List<NotesRequest> customerBookingNotes, String transactionId, boolean isDirectSync) throws RunnerException;

    void syncLockStatus(ShipmentDetails shipmentDetails);

    ResponseEntity<IRunnerResponse> syncFromBooking(ShipmentDetails shipmentDetails, List<UUID> deletedContGuids, List<NotesRequest> customerBookingNotes) throws RunnerException;
}
