package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@ToString
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ValidationResponse implements IRunnerResponse {
    Boolean success;
}
