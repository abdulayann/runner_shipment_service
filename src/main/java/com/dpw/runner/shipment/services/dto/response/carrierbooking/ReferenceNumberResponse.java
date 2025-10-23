package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReferenceNumberResponse implements IRunnerResponse {

    private Long id;   // from MultiTenancy (or BaseEntity)
    private UUID guid;
    private String type;
    private String referenceNumber;
    private Long carrierBookingId;
    private Long shippingInstructionId;
}
