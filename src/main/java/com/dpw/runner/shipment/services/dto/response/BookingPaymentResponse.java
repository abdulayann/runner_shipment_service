package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("Booking Payment Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BookingPaymentResponse implements IRunnerResponse {
    private String chargeType;
    private String paymentTerms;
    private Integer payer;
    private String paymentLocation;
}
