package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SendShipmentValidationResponse implements IRunnerResponse {
    private Boolean isError;
    private String shipmentErrorMessage;
}
