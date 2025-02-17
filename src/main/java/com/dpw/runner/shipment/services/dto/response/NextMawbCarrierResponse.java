package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Data;


@Data
@Builder
public class NextMawbCarrierResponse implements IRunnerResponse {
    private String nextMawbNumber;
}
