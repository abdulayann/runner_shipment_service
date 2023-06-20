package com.dpw.runner.shipment.services.commons.requests;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CommonGetRequest implements IRunnerRequest {
    private Long id;

}
