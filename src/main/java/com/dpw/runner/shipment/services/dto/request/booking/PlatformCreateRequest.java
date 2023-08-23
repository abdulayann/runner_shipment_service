package com.dpw.runner.shipment.services.dto.request.booking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@Data
public class PlatformCreateRequest extends CommonRequest implements IRunnerRequest {
}
