package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@ApiModel("Order Number Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ShipmentOrderRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID orderGuid;
    private Long shipmentId;
    private String orderNumber;
}
