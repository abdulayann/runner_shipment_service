package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.entity.enums.PayerType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FreightDetailRequest {
    private String chargeType;
    private String paymentTerms;
    private PayerType payerType;
    private String payerLocation;
    private Long shippingInstructionId;
}
