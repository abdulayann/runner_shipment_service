package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReferenceNumberResponse {

    private Long id;   // from MultiTenancy (or BaseEntity)
    private String type;
    private String referenceNumber;
    private Long carrierBookingId;
    private Long shippingInstructionId;
}
