package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ValidateSendShipmentRequest implements IRunnerRequest {
    private Long shipId;
}
