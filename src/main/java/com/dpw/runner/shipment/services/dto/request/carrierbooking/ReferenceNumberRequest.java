package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import lombok.Data;

@Data
public class ReferenceNumberRequest {
    private String type;
    private String referenceNumber;
    private Long carrierBookingId;
    private Long shippingInstructionId;
}
