package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import lombok.Data;

@Data
public class SyncBookingToService {
    private Long entityId; // carrierBookingID --> CarrierBooking --> console
    private String entityType; // carrierBooking
}
