package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import lombok.Data;

import javax.validation.constraints.NotBlank;


@Data
public class ReferenceNumberRequest {

    @NotBlank(message = "Type is required")
    private String type;

    @NotBlank(message = "Reference number is required")
    private String referenceNumber;

    private Long carrierBookingId;

    private Long shippingInstructionId;
}
