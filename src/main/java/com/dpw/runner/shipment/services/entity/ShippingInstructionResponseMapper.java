package com.dpw.runner.shipment.services.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionResponseMapper {
    private ShippingInstruction shippingInstruction;
    private String bookingStatus;
//    private String bookingNo;
}
