package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Data
@Schema("Order Number Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderNumberRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long orderNumber;
}
