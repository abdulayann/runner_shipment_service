package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ImportConsolidationResponse implements IRunnerResponse {
    String consolidationNumber;
    String message;
}
