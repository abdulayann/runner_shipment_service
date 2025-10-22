package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Getter
@Setter
@Schema(description = "Cache Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CacheRequest extends CommonRequest implements IRunnerRequest {
    private String key;
}
