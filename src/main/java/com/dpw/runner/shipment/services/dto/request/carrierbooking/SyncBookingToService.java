package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import lombok.Data;

@Data
public class SyncBookingToService {
    private String status;
    private String siStatus;
    private String crBookingId;
    private String mblNo;
    private String carrierBookingNo;

    //
    private Long entityId; // carrierBookingID --> CarrierBooking --> console
    private String entityType; // carrierBooking
}
