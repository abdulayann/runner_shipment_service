package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("Order Number Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentOrderResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private UUID orderGuid;
    private Long shipmentId;
    private String orderNumber;
}
