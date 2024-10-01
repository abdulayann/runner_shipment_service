package com.dpw.runner.shipment.services.dto.request;

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
public class ShipmentOrderAttachDetachRequest implements IRunnerRequest {
    private UUID orderGuid;
    private UUID shipmentGuid;
    private String orderNumber;
    private String event;
}
