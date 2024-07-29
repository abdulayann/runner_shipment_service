package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@ApiModel("Order Number Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderNumberRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long orderNumber;
}
