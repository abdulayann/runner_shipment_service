package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.PayerParties;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("Carrier Booking Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class BookingPaymentRequest extends CommonRequest implements IRunnerRequest {
    private String chargeType;
    private String paymentTerms;
    private PayerParties payer;
    private String paymentLocation;
}

