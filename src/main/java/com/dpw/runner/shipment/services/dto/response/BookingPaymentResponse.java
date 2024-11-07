package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.PayerParties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Booking Payment Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BookingPaymentResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String chargeType;
    private String paymentTerms;
    private PayerParties payer;
    private String paymentLocation;
    private Map<String, String> unlocationData;
    private Map<String, String> chargeTypeMasterData;
}
