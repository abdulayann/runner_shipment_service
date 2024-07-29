package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FnmStatusMessageResponse implements IRunnerResponse {
    private Boolean fnmStatus;
    private String response;
}
