package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Setter;

@Setter
@Builder
public class FnmStatusMessageResponse implements IRunnerResponse {
    String response;
}
